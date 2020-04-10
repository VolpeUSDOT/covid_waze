# From the data joined in Join_Data.R, train a model to generate expected counts of Waze alerts by alert type and county, at a daily level.
# Train the models on all available data from 2017 - 2019
# Generate expected counts for 2020, daily and by county
# Compare observed to expected to get an index of how affected a given county is by day.
# Output in a format which can be displayed in Tableau
# Runs in ~ 13 hrs on a 4 core, 16 gb RAM Windows machine.

# setup ----
library(tidyverse)
library(lubridate)
# library(lmer) # Maybe later: glmer models of Poisson counts, with county as random effect
library(randomForest) # for random forest models of counts 
library(foreach)
library(doParallel)

source('Analysis/RandomForest_WazeGrid_Fx.R')

load('Data/Waze_Covid_joined_2020-04-10.RData')
output.loc = 'Output'

#Use day of week (not day of year) to capture weekend/weekday patterns 
df$day_week <- wday(ymd(df$date),label = TRUE, abbr = FALSE)
df$weekend <- df$day_week == "Saturday" | df$day_week == "Sunday" 

#Save week of year
df$week_year <- week(df$date)

# All character vectors to factor for RF
col_is_char <- sapply(df, function(x) class(x)[1] == 'character', simplify = T)
for(c in 1:length(col_is_char)){
  this_col = col_is_char[c]
  if(this_col){
    df[,names(this_col)] <- as.factor(df[,names(this_col)])
   }
}

# Model expected counts ----
omits = c("state",
          "alert_type",
          "county",
          "cases",
          "deaths",
          "yearday",
          "date",
          "date_weekday",
          "week_year")

# crash_dat %>% group_by(state) %>% summarize(length(unique(fips))) %>% filter(state == 'TX')
# = 254 counties. 

# Now using month, dow, weekend, and FIPS as predictors. Will use count as response.
# randomForest is limited to 53 levels in a categorical variable. So fips as a single vector predictor won't work.
# FIPS are converted to columns, as a binary predictor. 
# Separate models by state and Waze alert type, with county dummy variables. 

# Another approach would be xgboost, which can handle even up to a Texas-sized number of counties (254) as a vector variable
# The advantage of modeling by state rather than each county seperately is the pooling effect of using more data together

# Random Forest inputs.
avail.cores = parallel::detectCores()
if(avail.cores > 8) avail.cores = 10 # 12 # To limit usage below max if on r4.4xlarge instance

rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = 8, maxnodes = 500, nodesize = 100)

# State loop ----

starttime = Sys.time()
keyoutputs = list()
# Fix error catching for DC -- 'data (x) has 0 rows...'

for(statex in unique(df$state)){
  # statex = 'RI'
  
  state_dat <- df %>%
    filter(state == statex)
  
  cs <- state_dat %>% 
    pivot_wider(names_from = fips,
                values_from = fips,
                names_prefix = 'f')

  fips_cols <- names(cs)[!names(cs) %in% names(df)] 
  
  class(cs) = 'data.frame'
  
  # Make these into factor binary variables
  for(i in 1:length(fips_cols)){
    cs[,fips_cols[i]] <- as.character(cs[,fips_cols[i]])
    
    cs[,fips_cols[i]][!is.na(cs[,fips_cols[i]])] = TRUE
    cs[,fips_cols[i]][is.na(cs[,fips_cols[i]])] = FALSE
    
    cs[,fips_cols[i]] <- as.factor(cs[,fips_cols[i]])
    
  }
 
  # Crashes 
  model.no = paste0('crash_', statex)
  
  crash_fit <- cs %>%
    filter(alert_type == 'ACCIDENT' & year != '2020')
  crash_pred <- cs %>%
    filter(alert_type == 'ACCIDENT' & year == '2020')
  
  keyoutputs[[model.no]] = do.rf(train.dat = crash_fit, 
                                 pred.dat = crash_pred,
                                 omits = omits, model.no = model.no,
                                 rf.inputs = rf.inputs) 

  # Weather/hazard 
  model.no = paste0('weh_', statex)
  
  weh_fit <- cs %>%
    filter(alert_type == 'WEATHERHAZARD' & year != '2020')
  weh_pred <- cs %>%
    filter(alert_type == 'WEATHERHAZARD' & year == '2020')
  
  keyoutputs[[model.no]] = do.rf(train.dat = weh_fit, 
                                 pred.dat = weh_pred,
                                 omits = omits, model.no = model.no,
                                 rf.inputs = rf.inputs) 

  # Jams 
  model.no = paste0('jam_', statex)
  
  jam_fit <- cs %>%
    filter(alert_type == 'JAM' & year != '2020')
  jam_pred <- cs %>%
    filter(alert_type == 'JAM' & year == '2020')
  
  keyoutputs[[model.no]] = do.rf(train.dat = jam_fit, 
                                 pred.dat = jam_pred,
                                 omits = omits, model.no = model.no,
                                 rf.inputs = rf.inputs) 
  
  timediff = Sys.time() - starttime
  cat('\n\n', statex, 'completed in', round(timediff, 2), attr(timediff, 'units'), rep('<<>>', 10), '\n\n')
  }

# Save outputs ----
model_names <- names(keyoutputs)
model_output_df <- vector()

for(m in model_names){
  mx <- as.data.frame(keyoutputs[[m]])
  state = unlist(strsplit(m, '_'))[2]
  var = unlist(strsplit(m, '_'))[1]
  
  model_output_df <- rbind(model_output_df, data.frame(model = m, state, var, mx))
}

save(file = file.path(output.loc, 'Model_fit_metrics.RData'),
     list = c('keyoutputs', 'model_output_df'))

write.csv(model_output_df, file = file.path(output.loc, 'Model_Output_Metrics.csv'), row.names = F)

# Examine model fit ----
# Bottom 10th percentile of MSE
model_output_df %>% filter(mse > quantile(mse, probs = seq(0, 1, 0.1))[9])

model_output_df %>% filter(mse < quantile(mse, probs = seq(0, 1, 0.1))[2])

ggplot(model_output_df) +
  geom_boxplot(aes(x = var, y = mse)) +
  facet_wrap(~var, scales = 'free_y')
  
gp1 <- ggplot(model_output_df, aes(color = state)) +
  geom_point(aes(x = Nobs_fit, y = mse)) +
  facet_wrap(~var, scales = 'free_y',
             nrow = 3) +
  ggtitle('Model fit (MSE) by size of training data for the three Waze alert types')

plotly::ggplotly(gp1)

# Loop over outputs to compile results ----
# Skip DC, currently don't have Waze data because was not saved correctly

compiled_pred <- vector()

for(statex in unique(df$state)[!unique(df$state) %in% c('DC')]){
  # statex = 'RI'
  models_to_read = paste0(c('crash_', 'weh_', 'jam_'), 
                          statex,
                          '_RandomForest_pred.csv')
  
  for(mod in models_to_read){
    state_pred <- read.csv(file.path(output.loc, mod), stringsAsFactors = F)
    
    state_pred <- state_pred %>%
      pivot_longer(cols = starts_with('f'),
                   names_to = 'fips',
                   names_prefix = 'f') %>%
      filter(value == TRUE)
    
    compiled_pred <- rbind(compiled_pred, state_pred)
  }
  
}

# Organize columns. Simplify the date columns
compiled_pred <- compiled_pred %>%
  select(-date, -date_weekday) %>%
  rename(date = yearday)

# Put in a sensible order
compiled_pred <- compiled_pred %>%
  select(date,
         state,
         county,
         alert_type,
         count,
         pred_count,
         cases,
         deaths,
         fips,
         year,
         month,
         dow,
         day_week,
         weekend,
         week_year
         )
# Save compiled predictions
save(file = file.path(output.loc, 'Waze_2020_Predicted_Observed.RData'),
     list = c('compiled_pred'))

write.csv(compiled_pred, file = file.path(output.loc, 'Waze_2020_Predicted_Observed.csv'), row.names = F)

