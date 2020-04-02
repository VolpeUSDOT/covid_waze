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
omits = c("alert_type",
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
# The advantage of modeling by state rather than each county seperately is the pooling effect of using more data together


model.no = "crash_01"

crash_dat <- df %>%
  filter(alert_type == "ACCIDENT" &
           year != '2020') %>%
  sample_n(1000)

cs <-  crash_dat %>% 
  pivot_wider(key = fips,
              value = fips)

fips_cols <- names(cs)[!names(cs) %in% names(df)] 
# Make these into factor binary variables
for(i in 1:length(fips_cols)){
  cs[,fips_cols[i]][!is.na(cs[,fips_cols[i]])] = TRUE
  cs[,fips_cols[i]][is.na(cs[,fips_cols[i]])] = FALSE
  }

cra_mod <- do.rf(crash_dat)