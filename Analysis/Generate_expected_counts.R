# From the data joined in Join_Data.R, train a model to generate expected counts of Waze alerts by alert type and county, at a daily level.
# Train the models on all available data from 2017 - 2019
# Generate expected counts for 2020, daily and by county
# Compare observed to expected to get an index of how affected a given county is by day.
# Output in a format which can be displayed in Tableau

# setup ----
library(tidyverse)
library(lubridate)
# library(lmer) # for glmer models of Poisson counts, with county as random effect
library(randomForest) # for random forest models of counts for comparison (maybe)
library(foreach)
library(doParallel)

teambucket <- "s3://prod-sdc-sdi-911061262852-us-east-1-bucket"

source('Analysis/RandomForest_WazeGrid_Fx.R')

load('Data/Waze_Covid_joined.RData')
output.loc = 'Output'


#Use day of week (not day of year) to capture weekend/weekday patterns 
df$date_weekday <- as.Date(df$date)
df$day_week <- wday(ymd(df$date_weekday),label = TRUE, abbr = FALSE)
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

# Will use year, month, dow, day_week, and weekend as predictors. Will use count as response.
# randomForest is limited to 53 levels in a categorical variable. So fips as a predictor won't work.
# Fips could be converted to columns, as a binary predictor. Testing this approach now.

# Since we ultimately want predictions at the county level, the simplest approach is to model each county separately.
# Another approach would be xgboost, which can handle even up to a Texas-sized number of counties (254)
# Now using state models, with county dummy variables
# The advantage of modeling by state rather than each county seperately is the pooling effect of using more data together

# Random Forest inputs.
avail.cores = parallel::detectCores()
if(avail.cores > 8) avail.cores = 10 # 12 # To limit usage below max if on r4.4xlarge instance

rf.inputs = list(ntree.use = avail.cores * 50, avail.cores = avail.cores, mtry = 10, maxnodes = 1000, nodesize = 100)

# Fail ---- 
# integer overflow when trying this -- too many FIPS to convert easily to binary encoding using pivot_wider
# Switching to state model approach, keeping this for reference

# model.no = "crash_01"

# crash_dat <- df %>%
#   filter(alert_type == "ACCIDENT" &
#            year != '2020') # %>% sample_n(1000)
# cs <-  crash_dat %>% 
#   pivot_wider(names_from = fips,
#               values_from = fips,
#               names_prefix = 'f')
# 
# fips_cols <- names(cs)[!names(cs) %in% names(df)] 
# 
# class(cs) = 'data.frame'
# 
# # Make these into factor binary variables
# for(i in 1:length(fips_cols)){
#   cs[,fips_cols[i]] <- as.character(cs[,fips_cols[i]])
#   
#   cs[,fips_cols[i]][!is.na(cs[,fips_cols[i]])] = TRUE
#   cs[,fips_cols[i]][is.na(cs[,fips_cols[i]])] = FALSE
# 
#   cs[,fips_cols[i]] <- as.factor(cs[,fips_cols[i]])
#   
#   }

# State loop ----

keyoutputs = list()

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
                                 omits = omits, model.no = model.no) 

  # Weather/hazard 
  model.no = paste0('weh_', statex)
  
  weh_fit <- cs %>%
    filter(alert_type == 'WEATHERHAZARD' & year != '2020')
  weh_pred <- cs %>%
    filter(alert_type == 'WEATHERHAZARD' & year == '2020')
  
  keyoutputs[[model.no]] = do.rf(train.dat = weh_fit, 
                                 pred.dat = weh_pred,
                                 omits = omits, model.no = model.no) 

  # Jams 
  model.no = paste0('jam_', statex)
  
  jam_fit <- cs %>%
    filter(alert_type == 'JAM' & year != '2020')
  jam_pred <- cs %>%
    filter(alert_type == 'JAM' & year == '2020')
  
  keyoutputs[[model.no]] = do.rf(train.dat = jam_fit, 
                                 pred.dat = jam_pred,
                                 omits = omits, model.no = model.no) 
  
  }

