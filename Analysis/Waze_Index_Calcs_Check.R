# Check index calculations checks for Waze-COVID dashboard
# https://internal.explore.dot.gov/#/views/WazeCOVID/HomeDashboard?:iid=1

# After fetch_fresh_data.R
# Calculate weekly values for overall Waze data, Safety Index, and Congestion Index
# For Previous week of 2020, Same week of 2019, and compared to early 2020 baseline

# setup ----
library(tidyverse)
library(readr)

input.loc = 'Data'
output.loc = 'Output'

latest_refresh_day = max(dir('Output')[grep('2020-', dir('Output'))]) # e.g. '2020-05-06'

d <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_Index_cleaned.csv'),
              col_types = cols(cases = col_double(),
                               deaths = col_double()))       

nw <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_National_week.csv'))       


# Daily CSA

d_csa <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_MSA_day.csv'),
                  col_types = cols(Metropolitan.Division.Code = col_character(),
                                   Metropolitan.Division.Title = col_character(),
                                   cases = col_double(),
                                   deaths = col_double(),
                                   dowavg19_ACCIDENT = col_double(),
                                   dowavg19_JAM = col_double(),
                                   dowavg19_WEATHERHAZARD = col_double(),
                                   bl2020_mean_ACCIDENT = col_double(),
                                   bl2020_mean_JAM = col_double(),
                                   bl2020_mean_WEATHERHAZARD = col_double()
                                   ))   


# Summarize to week ----

week_index_calcs <- nw %>%
  group_by(week) %>%
  summarize(pct_ch_from_prev_week_crash = (weeksum20_ACCIDENT - lag1_weeksum20_ACCIDENT) / lag1_weeksum20_ACCIDENT)
