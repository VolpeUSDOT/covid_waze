# Sanity checks for Waze-COVID data

# To check after fetch_fresh_data.R
# 1. Are there days with all NA for Waze data across all counties?
# 2. Are there counties with non-NA data that have day-to-day changes of more than 200%? Other %?
# 3. What is the last complete day of data? For both joined and full data sets.

# Consider: make this into an RMarkdown file which is generated after fetching fresh data.

# setup ----
library(tidyverse)
library(readr)

input.loc = 'Data'
output.loc = 'Output'

latest_refresh_day = max(dir('Output')[grep('2020-', dir('Output'))]) # e.g. '2020-05-06'

d_full <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_Covid_joined.csv'),
                   col_types = cols(cases = col_double(),
                                    deaths = col_double()))    

d <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_Index_cleaned.csv'),
              col_types = cols(cases = col_double(),
                               deaths = col_double()))       

# Summarize for tests ----

date_count <- d %>%
  group_by(date) %>%
  summarize(total_Waze_count = sum(count_ACCIDENT, count_JAM, count_WEATHERHAZARD, na.rm = T),
            count_NA = sum(is.na(count_ACCIDENT), is.na(count_JAM), is.na(count_WEATHERHAZARD)))


date_count_full <- d_full %>%
  group_by(date) %>%
  summarize(total_Waze_count = sum(count, na.rm = T),
            count_NA = sum(is.na(count)))


co_date_change

d = d[order(d$fips, d$date),]

d$change_crash = with(d, (count_ACCIDENT - dplyr::lag(count_ACCIDENT) ) / count_ACCIDENT)
d$change_jam = with(d, (count_JAM - dplyr::lag(count_JAM) ) / count_JAM)
d$change_weh = with(d, (count_WEATHERHAZARD - dplyr::lag(count_WEATHERHAZARD) ) / count_WEATHERHAZARD)

d$mean_change = rowSums(d[,c('change_crash', 'change_jam', 'change_weh')], na.rm = T)/3

# test
d %>% dplyr::select(fips, date, count_ACCIDENT, change_crash, count_JAM, change_jam, mean_change) %>% 
  filter(fips == '01097')


co_date_count_full <- d_full %>%
  group_by(fips, date, state, county) %>%
  summarize(total_Waze_count = sum(count, na.rm = T),
            count_NA = sum(is.na(count)))


# Assess completeness ---- 

# How many counties had days with mean daily change in total Waze counts of > 200%? > 500%?

d_change = d %>%
  ungroup() %>%
  group_by(fips) %>%
  summarize(n_daily_change_200 = sum(abs(mean_change) > 2),
            n_daily_change_500 = sum(abs(mean_change) > 5),
            n_daily_change_1000 = sum(abs(mean_change) > 10))

hist(d_change$n_daily_change_500)
  
n_counties_with_200_change = nrow(d_change %>% filter(n_daily_change_200 > 10)) 
n_counties_with_500_change = nrow(d_change %>% filter(n_daily_change_500 > 10)) 
n_counties_with_1000_change = nrow(d_change %>% filter(n_daily_change_1000 > 10)) 
n_counties_total = nrow(d_change)

cat(n_counties_with_200_change, 'of', n_counties_total, 'counties had more than 10 days with a daily total change in Waze events of at least +/- 200%')

cat(n_counties_with_500_change, 'of', n_counties_total, 'counties had more than 10 days with a daily total change in Waze events of at least +/- 500%')

cat(n_counties_with_1000_change, 'of', n_counties_total, 'counties had more than 10 days with a daily total change in Waze events of at least +/- 1000%')

# Compare baseline threshold and non-threshold ----

co_NA_count = d %>%
  group_by(fips) %>%
  summarize(NA_crash_threshold = sum(is.na(bl2020_mean_ACCIDENT)),
            NA_jam_threshold = sum(is.na(bl2020_mean_JAM)),
            NA_weh_threshold = sum(is.na(bl2020_mean_WEATHERHAZARD)),
            NA_crash_nonthreshold = sum(is.na(bl2020_mean_ACCIDENT_nf)),
            NA_jam_nonthreshold = sum(is.na(bl2020_mean_JAM_nf)),
            NA_weh_nonthreshold = sum(is.na(bl2020_mean_WEATHERHAZARD_nf))
  )

# Test
d %>% filter(fips == '01005') %>% select(fips, date, bl2020_mean_ACCIDENT, bl2020_mean_ACCIDENT_nf)

ggplot(co_NA_count, aes(x = NA_crash_threshold, y = NA_crash_nonthreshold)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2)

# plot ----

ggplot(date_count, aes(x = date, y = total_Waze_count)) +
  geom_line() +
  ggtitle('Total Waze events by date')

ggplot(date_count, aes(x = date, y = count_NA)) +
  geom_line() +
  ggtitle('Total counties x alert types with NA Waze data by date')


ggplot(date_count_full, aes(x = date, y = total_Waze_count)) +
  geom_line() +
  ggtitle('Total Waze events by date \n Full data set')

ggplot(date_count_full, aes(x = date, y = count_NA)) +
  geom_line() +
  ggtitle('Total counties x alert types with NA Waze data by date \n Full data set')

