# Check index calculations checks for Waze-COVID dashboard
# https://internal.explore.dot.gov/#/views/WazeCOVID/HomeDashboard?:iid=1

# After fetch_fresh_data.R
# Calculate weekly values for overall Waze data, Safety Index, and Congestion Index
# For Previous week of 2020, Same week of 2019, and compared to early 2020 baseline


# TODO
# HOME: Index_cleaned for climate stripe
# National week for hex and for triangles

# Action: click on a state, go to state Dashboard and filter based on State.

# For MSA_week and MSA_day:
# Filter out to only Central.Outlying.County == central (maybe not necessary because summing across counties)
# Filter out Micropolitan SAs
# Sum up by day / week for each CBSA
# DUPLICATE that row for each relevant state
# The resulting files will no longer have fips, county, or Central.Outlying.County fields.

# setup ----
library(tidyverse)
library(readr)

input.loc = 'Data'
output.loc = 'Output'

latest_refresh_day = max(dir('Output')[grep('2020-', dir('Output'))]) # e.g. '2020-05-06'

nw <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_National_week.csv'))       
nd <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_National_day.csv'))       

 
# Daily MSA

d_MSA_day <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_MSA_day.csv'),
                  col_types = cols(dowavg19_ACCIDENT = col_double(),
                                   dowavg19_JAM = col_double(),
                                   dowavg19_WEATHERHAZARD = col_double(),
                                   bl2020_mean_ACCIDENT = col_double(),
                                   bl2020_mean_JAM = col_double(),
                                   bl2020_mean_WEATHERHAZARD = col_double()
                                   ))

# Weekly MSA

d_MSA_week <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_MSA_week.csv'),
                      col_types = cols(weeksum19_ACCIDENT = col_double(),
                                       weeksum19_JAM = col_double(),
                                       weeksum19_WEATHERHAZARD = col_double(),
                                       bl2020_mean_ACCIDENT = col_double(),
                                       bl2020_mean_JAM = col_double(),
                                       bl2020_mean_WEATHERHAZARD = col_double()
                      ))


# Summarize to week ----

# in process -- also do 2019 measures and early 2020 baseline
week_index_calcs <- nw %>%
  group_by(week) %>%
  summarize(sum_crash_20 = sum(weeksum20_ACCIDENT_nf, na.rm = T),
            sum_weh_20 = sum(weeksum20_WEATHERHAZARD_nf, na.rm = T),
            sum_jam_20 = sum(weeksum20_JAM_nf, na.rm = T),
            sum_crash_20_lag = sum(lag1_weeksum20_ACCIDENT_nf, na.rm = T),
            sum_weh_20_lag = sum(lag1_weeksum20_WEATHERHAZARD_nf, na.rm = T),
            sum_jam_20_lag = sum(lag1_weeksum20_JAM_nf, na.rm = T),
            
            pct_ch_from_prev_week_crash = (sum_crash_20 - sum_crash_20_lag) / sum_crash_20_lag)

View(week_index_calcs)

# Sanity checks ----

nd %>% filter(state == 'CA') %>% View()
nw %>% filter(state == 'CA') %>% View()

nw_sum <- nw %>% 
  group_by(week) %>%
  summarize(cra = sum(weeksum20_ACCIDENT, na.rm = T),
            jam = sum(weeksum20_JAM, na.rm = T),
            weh = sum(weeksum20_WEATHERHAZARD, na.rm = T),
            Total_Waze_20 = sum(cra, jam, weh),
            Total_Waze_BL = sum(bl2020_mean_ACCIDENT, bl2020_mean_JAM, bl2020_mean_WEATHERHAZARD, na.rm = T),
            pct_ch = ( Total_Waze_20 - Total_Waze_BL ) / Total_Waze_BL)

ggplot(nw_sum, aes(x = week, y = Total_Waze_20)) + geom_line()

ggplot(nw_sum, aes(x = week, y = cra)) + geom_line()

ggplot(nw_sum, aes(x = week, y = weh)) + geom_line()


