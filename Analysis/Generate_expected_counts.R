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

# Model expected counts ----
omits = c("alert_type",
          "county",
          "cases",
          "deaths",
          "yearday",
          "date",
          "date_weekday",
          "week_year")

# Will use fips, state, year, month, dow, day_week, and weekend as predictors. Will use count as response.

model.no = "crash_01"

crash_dat <- df %>%
  filter(alert_type == "ACCIDENT") 

cra_mod <- do.rf(crash_dat)