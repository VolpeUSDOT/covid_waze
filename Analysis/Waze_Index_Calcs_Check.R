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

setwd("~/GitHub/covid_waze/") #Erika needed

input.loc = 'Data'
output.loc = 'Output'

drive.output = '//vntscex.local/DFS/Projects/PROJ-OS62A1/SDI Waze Phase 2/Data/COVID'

latest_refresh_day = max(dir('Output')[grep('2020-', dir('Output'))]) # e.g. '2020-05-06'

cat(latest_refresh_day)

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

# <<>><<>><<>><<>>
# Weighted percent change at a national level. Below is the Tableau calculation for reference
 
# WoY - %CHANGE - JAMS - COUNTRY nf
# 
# Role:
#   Continuous Measure
# Type:
#   Calculated Field
# Status:
#   Valid
# 
# Formula
# // state factor is the ratio between the individual state and other states
# // difference between the same week in 2019 and the sum of jams events * state factor
# 
# 
# // percent change of crashes from comparable week in 2019.
# 
# (([_WoY WEIGHT - JAMS - STATE nf]
#   * (SUM([weeksum20 JAM nf]) - SUM([weeksum19 JAM nf])) 
#   / SUM([weeksum19 JAM nf]))) * 100
# 
# 
# // NOT FILTERED -- WEIGHTED
# The domain for this field has not been loaded. Click "Load" to retrieve

# <<>><<>><<>><<>>
# Where the week of year Weight field is calculated as follows:

# _WoY WEIGHT - JAMS - STATE nf
# 
# Role:
#   Continuous Measure
# Type:
#   Calculated Field
# Status:
#   Valid
# 
# Formula
# // state-based factor for change from same week in 2019
# // this value is then multiplied by the sum of jams within a state
# 
# // this uses the unfiltered (raw) baseline state data
# 
# 
# SUM([weeksum19 JAM nf])
# / SUM({ EXCLUDE [State Abbr] : SUM([weeksum19 JAM nf]) } )
# The domain for this field has not been loaded. Click "Load" to retrieve.

nw <- nw %>%
  ungroup %>%
  group_by(week) %>%
    mutate(WoY_Weight_Jams_19 = weeksum19_JAM_nf / sum(weeksum19_JAM_nf, na.rm=T),
           WoY_Weight_Crash_19 = weeksum19_ACCIDENT_nf / sum(weeksum19_ACCIDENT_nf, na.rm=T),
           
           WoY_Weight_Jams_bl = bl2020_mean_JAM_nf / sum(bl2020_mean_JAM_nf, na.rm=T),
           WoY_Weight_Crash_bl = bl2020_mean_ACCIDENT_nf / sum(bl2020_mean_ACCIDENT_nf, na.rm=T),
           
           WoY_Weight_Jams_lag1 = lag1_weeksum20_JAM_nf / sum(lag1_weeksum20_JAM_nf, na.rm=T),
           WoY_Weight_Crash_lag1 = lag1_weeksum20_ACCIDENT_nf / sum(lag1_weeksum20_ACCIDENT_nf, na.rm=T)
           ) %>%
  ungroup()

# View(nw)

# Test: CA in week 6 had these characteristics 
# WoY_Weight_Crash_19: 0.33808351
# WoY_Weight_Crash_bl: 0.29436261
# WoY_Weight_Crash_lag1: 0.31452333

ca_6 <- nw %>% filter(state == 'CA'  & week == '6') %>%
  select(weeksum19_ACCIDENT, bl2020_mean_ACCIDENT, lag1_weeksum20_ACCIDENT)

# and totals 
total_6 <- nw %>% filter(week == '6') %>%
  summarize(sum(weeksum19_ACCIDENT, na.rm = T), 
            sum(bl2020_mean_ACCIDENT, na.rm = T),
            sum(lag1_weeksum20_ACCIDENT, na.rm = T))

ca_6 / total_6 # checks out

# First calculate the percent changes for *each* state, each week
# Then use the weights, calculated above, to produce a weighted mean of the state-wise percent changes at the national level, by week

week_index_calcs <- nw %>%
  ungroup() %>%
  mutate(
    pct_ch_from_prev_week_jam = 100 *  ( (weeksum20_JAM_nf - lag1_weeksum20_JAM_nf) / lag1_weeksum20_JAM_nf ),
    pct_ch_from_prev_week_crash = 100 *  ( (weeksum20_ACCIDENT_nf - lag1_weeksum20_ACCIDENT_nf) / lag1_weeksum20_ACCIDENT_nf ),

    pct_ch_from_2019_week_jam = 100 *  ( (weeksum20_JAM_nf - weeksum19_JAM_nf) / weeksum19_JAM_nf ),
    pct_ch_from_2019_week_crash = 100 *  ( (weeksum20_ACCIDENT_nf - weeksum19_ACCIDENT_nf) / weeksum19_ACCIDENT_nf ),

    pct_ch_from_2020bl_week_jam = 100 *  ( (weeksum20_JAM_nf - bl2020_mean_JAM_nf) / bl2020_mean_JAM_nf ),
    pct_ch_from_2020bl_week_crash = 100 *  ( (weeksum20_ACCIDENT_nf - bl2020_mean_ACCIDENT_nf) / bl2020_mean_ACCIDENT_nf )
    
  ) %>%
  group_by(week) %>%
  summarize(
    
    weekly_sum_20_jam = sum(weeksum20_JAM_nf, na.rm = T),
    
    weekly_sum_19_jam = sum(weeksum19_JAM_nf, na.rm = T),

    change_from_baseline_jam = 100 * (weekly_sum_20_jam - weekly_sum_19_jam) / weekly_sum_19_jam,
    
    pct_ch_from_prev_week_jam = weighted.mean(pct_ch_from_prev_week_jam, w = WoY_Weight_Jams_lag1),
    pct_ch_from_prev_week_crash = weighted.mean(pct_ch_from_prev_week_crash, w = WoY_Weight_Crash_lag1),
    
    pct_ch_from_2019_week_jam = weighted.mean(pct_ch_from_2019_week_jam, w = WoY_Weight_Jams_19),
    pct_ch_from_2019_week_crash = weighted.mean(pct_ch_from_2019_week_crash, w = WoY_Weight_Crash_19),
    
    pct_ch_from_2020bl_week_jam = weighted.mean(pct_ch_from_2020bl_week_jam, w = WoY_Weight_Jams_bl),
    pct_ch_from_2020bl_week_crash = weighted.mean(pct_ch_from_2020bl_week_crash, w = WoY_Weight_Crash_bl),
    
    ) 
  
# View(week_index_calcs)

# Write this weekly index file out to the Volpe shared drive

write.csv(week_index_calcs, file = file.path(drive.output, 'Weekly_Covid_Outputs.csv'), row.names = F)


# Output table for COVID-19 Passenger Impact ----

# Week 
# Lowest (count of jams)
# Peak (count of jams)
# Current (count of jams)
# Baseline (count of jams)
# Change from baseline (this week of 2019)

output_table = week_index_calcs %>%
  filter(week == max(week)) %>%
  select(week, weekly_sum_20_jam, weekly_sum_19_jam, change_from_baseline_jam)

lowest = week_index_calcs %>%
  filter(week >= 10) %>%
  filter(weekly_sum_20_jam == min(weekly_sum_20_jam)) %>%
  select(week, weekly_sum_20_jam) %>%
  rename(week_of_lowest = week, 
         lowest_weekly_sum_20_jam = weekly_sum_20_jam)

peak = week_index_calcs %>%
  filter(week >= lowest$week_of_lowest) %>%
  filter(weekly_sum_20_jam == max(weekly_sum_20_jam)) %>%
  select(week, weekly_sum_20_jam) %>%
  rename(week_of_peak = week, 
         peak_weekly_sum_20_jam = weekly_sum_20_jam)


output = data.frame(output_table, lowest, peak)

write.csv(output, file = file.path(drive.output, 'Output_for_BTS.csv'), row.names = F, append = T)


output = data.frame(output_table, lowest, peak)

write.csv(output, file = file.path(drive.output, 'Output_for_BTS.csv'), row.names = F, append = T)

# Sanity checks ----

#nd %>% filter(state == 'CA') %>% View()
#nw %>% filter(state == 'CA') %>% View()

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


