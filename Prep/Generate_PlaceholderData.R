# Generate placeholder data for Tableau Dashboard of Waze / Covid case data 
# Expected Waze counts, Restrictions

# setup ----
library(tidyverse)
library(egg)
library(lubridate)

load('Data/Waze_Covid_joined.RData')
output.loc = 'Output'

# Histograms by alert type
ggplot(df) + 
  geom_histogram(aes(count)) + 
  scale_x_log10() +
  facet_wrap(~alert_type)

# check df size
dim(df)
names(df)
str(df)

#Generate placeholders for expected values by county/day for 2020 based on 2018 and 2019 data

#Use day of week (not day of year) to capture weekend/weekday patterns 
df$date_weekday <- as.Date(df$date)
df$day_week <- wday(ymd(df$date_weekday),label = TRUE, abbr = FALSE)
df$weekend <- df$day_week == "Saturday" | df$day_week == "Sunday" 

#Save week of year
df$week_year <- week(df$date)

#Placeholder values - county level averages by month and dow
df_no2020 <- filter(df, year != "2020")
waze_avg <- df_no2020 %>%
  group_by(alert_type, fips, state, county, month, day_week) %>%
  summarise(obs_count_mean = round(mean(count, na.rm=TRUE),1), n = n())

write.csv(waze_avg, file=file.path(output.loc,"waze_avg_counts.csv"), row.names=FALSE)

#Join averages by alert type, county, month, day_week to df as placeholders
#waze_estimates_df <- left_join(df, waze_avg, by = c('alert_type','fips', 'state', 'county', 'month', 'day_week'), keep=FALSE)

#New estimated model data 
#Observed-predicted (%change for each type, then average of percent changes)
#Hex map for state - then click on state to get county data, then click county to get line graphs