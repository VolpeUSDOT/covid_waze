# Generates weekly index calculations for BTS dashboard
# Also serves as checks of index calculations checks for Waze-COVID dashboard
# https://internal.explore.dot.gov/#/views/WazeCOVID/HomeDashboard?:iid=1

# After fetch_fresh_data.R
# Calculate weekly values for overall Waze data, Safety Index, and Congestion Index
# For Previous week of 2020, Same week of 2019, and compared to early 2020 baseline

# Output csv file 

# setup ----
library(tidyverse)
library(lubridate)
library(readr)

# setwd("~/GitHub/covid_waze/") #Erika needed -- don't need if you launch from covid_waze.Rproj, sets working directory to location of the covid_waze repository

input.loc = 'Data'
output.loc = 'Output'

drive.output = '//vntscex.local/DFS/Projects/PROJ-OS62A1/SDI Waze Phase 2/Data/COVID'

latest_refresh_day = max(dir('Output')[grep(format(Sys.Date(), '%Y'), dir('Output'))]) # e.g. '2020-05-06'

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


nw <- nw %>%
  ungroup %>%
  group_by(year, week) %>%
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

ca_6 <- nw %>% filter(state == 'CA'  & week == '6' & year == '2020') %>%
  select(weeksum19_ACCIDENT, bl2020_mean_ACCIDENT, lag1_weeksum20_ACCIDENT)

# and totals 
total_6 <- nw %>% filter(week == '6' & year == '2020') %>%
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
  group_by(year, week) %>%
  summarize(
    
    weekly_sum_20_jam = sum(weeksum20_JAM_nf, na.rm = T),
    
    weekly_sum_19_jam = sum(weeksum19_JAM_nf, na.rm = T),

    change_from_19_jam = 100 * (weekly_sum_20_jam - weekly_sum_19_jam) / weekly_sum_19_jam,
    
    pct_ch_from_prev_week_jam = weighted.mean(pct_ch_from_prev_week_jam, w = WoY_Weight_Jams_lag1),
    #pct_ch_from_prev_week_crash = weighted.mean(pct_ch_from_prev_week_crash, w = WoY_Weight_Crash_lag1),
    
    pct_ch_from_2019_week_jam = weighted.mean(pct_ch_from_2019_week_jam, w = WoY_Weight_Jams_19),
    #pct_ch_from_2019_week_crash = weighted.mean(pct_ch_from_2019_week_crash, w = WoY_Weight_Crash_19),
    
    pct_ch_from_2020bl_week_jam = weighted.mean(pct_ch_from_2020bl_week_jam, w = WoY_Weight_Jams_bl),
    #pct_ch_from_2020bl_week_crash = weighted.mean(pct_ch_from_2020bl_week_crash, w = WoY_Weight_Crash_bl),
    
    ) 
  
# View(week_index_calcs)


# Write this weekly index file out to the Volpe shared drive

write.csv(week_index_calcs, file = file.path(drive.output, 'Weekly_Covid_Outputs.csv'), row.names = F)


# Output table for COVID-19 Passenger Impact ----


# Week number is generated by lubridate::epiweek. From ?epiweek
# "epiweek() is the US CDC version of epidemiological week. It follows same rules as isoweek() but starts on Sunday. In other parts of the world the convention is to start epidemiological weeks on Monday, which is the same as isoweek."

# To convert this back to a date for the end of the week, figure out what date the first Sunday for which a week was at least 4 days into 2020. (Dec 29 of 2019), then add 7*weeknumber days to get the day of year for the Saturday that ended each week. Convert to date-time by appending 2020 and using %j format for day of year
# This calculated 'isoweek', but we are using epiweeks
# first_week = paste('2020', '01', formatC(1:7, width = 2, flag = '0'), sep = '-')
# first_week = strptime(first_week, format = '%Y-%m-%d')
# first_Sunday = first_week[wday(first_week, label = TRUE, abbr = FALSE) == 'Sunday']
# first_Sunday = strptime('2019-12-29', format = '%Y-%m-%d')
# 
# first_Saturday = first_Sunday + 6 * (60*60*24) # because date-time objects are measured in seconds
# 
# week_ending_date = as.Date(first_Saturday + ((week_index_calcs$week -1 ) * 7 * (60*60*24)))
# 
# week_index_calcs$week_ending_date = week_ending_date

# Above worked only for single year, now take new approach for 2020 - present day

# look for leap years
days_in_year_20 = ifelse(lubridate::days_in_month('2020-02-01') == 29, 366, 365)
days_in_year_21 = ifelse(lubridate::days_in_month('2021-02-01') == 29, 366, 365)

dates = c(paste('2020', formatC(1:days_in_year_20, width = 2, flag = '0'), sep = '-'),
          paste('2021', formatC(1:days_in_year_21, width = 2, flag = '0'), sep = '-'))

dates = strptime(dates, '%Y-%j')
week = lubridate::epiweek(dates)
weeklookup = data.frame(dates, year = format(dates, '%Y'), week)

week_end_date = weeklookup %>% 
  mutate(weekchange = week - lead(week)) %>% 
  filter(weekchange != 0) %>%
  select(-weekchange) %>%
  mutate(year = as.numeric(year),
         week = as.numeric(week)) %>%
  rename(week_ending_date = dates)

week_index_calcs = week_index_calcs %>% 
  left_join(week_end_date)

# For each week, calculate the following at the time of that week

# Week 
# Lowest (count of jams)
# Peak (count of jams)
# Current (count of jams)
# Baseline (count of jams)
# Change from baseline (this week of 2019)


# Below is code to just calculate one row, the latest week
this_year = format(Sys.Date(), '%Y')

output_table = week_index_calcs %>%
  filter(year == this_year) %>%
  filter(week == max(week)) %>% 
  select(week, week_ending_date, weekly_sum_20_jam, weekly_sum_19_jam, change_from_19_jam)

lowest = week_index_calcs %>%
  filter((week >= 10 & year == '2020') | year == '2021') %>%  
  ungroup() %>%
  filter(weekly_sum_20_jam == min(weekly_sum_20_jam)) %>%
  select(year, week, weekly_sum_20_jam) %>%
  rename(year_of_lowest = year,
         week_of_lowest = week, 
         lowest_weekly_sum_20_jam = weekly_sum_20_jam)

peak = week_index_calcs %>%
  ungroup() %>%
  filter((week >= lowest$week_of_lowest & year == '2020') | year == '2021') %>%
  filter(weekly_sum_20_jam == max(weekly_sum_20_jam)) %>%
  select(year, week, weekly_sum_20_jam) %>%
  rename(year_of_peak = year,
         week_of_peak = week, 
         peak_weekly_sum_20_jam = weekly_sum_20_jam)

output = data.frame(output_table, lowest, peak)

write.table(output, file = file.path(drive.output, 'Output_for_BTS_National_2021.csv'), 
            row.names = F, col.names = F, 
            sep = ",", qmethod = "double", append = T)

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


