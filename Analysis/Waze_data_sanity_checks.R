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

refresh_days = dir('Output')[grep(format(Sys.Date(), '%Y'), dir('Output'))] # e.g. '2020-05-06'
latest_refresh_day = max(as.Date(refresh_days[!grepl('.zip', refresh_days)]))


d_full <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_Full.csv'),
                   col_types = cols(cases = col_double(),
                                    deaths = col_double()))    

# d <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_Index_cleaned.csv'),
#               col_types = cols(cases = col_double(),
#                                deaths = col_double()))       

nw <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_National_week.csv'))       


# Daily MSA

d_msa <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_MSA_day.csv'),
                  col_types = cols(dowavg19_ACCIDENT = col_double()))   


# Summarize for tests ----

date_count <- d_msa %>%
  group_by(date) %>%
  summarize(total_Waze_count = sum(count_ACCIDENT, count_JAM, count_WEATHERHAZARD, na.rm = T),
            count_NA = sum(is.na(count_ACCIDENT), is.na(count_JAM), is.na(count_WEATHERHAZARD)))


# date_count_full <- d_full %>%
#   group_by(date) %>%
#   summarize(total_Waze_count = sum(count, na.rm = T),
#             count_NA = sum(is.na(count)))


d = d_msa

d = d[order(d$CBSA.Code, d$date),]

d$change_crash = with(d, (count_ACCIDENT - dplyr::lag(count_ACCIDENT) ) / count_ACCIDENT)
d$change_jam = with(d, (count_JAM - dplyr::lag(count_JAM) ) / count_JAM)
d$change_weh = with(d, (count_WEATHERHAZARD - dplyr::lag(count_WEATHERHAZARD) ) / count_WEATHERHAZARD)

d$mean_change = rowSums(d[,c('change_crash', 'change_jam', 'change_weh')], na.rm = T)/3

# test
d %>% dplyr::select(CBSA.Code, date, count_ACCIDENT, change_crash, count_JAM, change_jam, mean_change) %>% 
  filter(grepl('Philadelphia', d$CBSA.Title))


# co_date_count_full <- d_full %>%
#   group_by(fips, date, state, county) %>%
#   summarize(total_Waze_count = sum(count, na.rm = T),
#             count_NA = sum(is.na(count)))


co_date_count_full <- d_full %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(total_Waze_count = sum(count, na.rm = T),
            count_NA = sum(is.na(count)))

gp <- ggplot(co_date_count_full, aes(x = date, y = total_Waze_count)) + 
  geom_line() +
  xlim(as.Date(c('2021-01-01', Sys.Date())))
plotly::ggplotly(gp)

ggplot(co_date_count_full, aes(x = date, y = total_Waze_count)) + 
  geom_line()

# Last 10 days
co_date_count_full[(nrow(co_date_count_full)-10):nrow(co_date_count_full),]

# Assess completeness ---- 

# How many counties had days with mean daily change in total Waze counts of > 200%? > 500%?

d_change = d %>%
  ungroup() %>%
  group_by(CBSA.Code) %>%
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
  group_by(CBSA.Code) %>%
  summarize(NA_crash_threshold = sum(is.na(bl2020_mean_ACCIDENT)),
            NA_jam_threshold = sum(is.na(bl2020_mean_JAM)),
            NA_weh_threshold = sum(is.na(bl2020_mean_WEATHERHAZARD)),
            NA_crash_nonthreshold = sum(is.na(bl2020_mean_ACCIDENT_nf)),
            NA_jam_nonthreshold = sum(is.na(bl2020_mean_JAM_nf)),
            NA_weh_nonthreshold = sum(is.na(bl2020_mean_WEATHERHAZARD_nf))
  )

# Test
d %>% filter(CBSA.Code == '47900') %>% select(CBSA.Code, date, 
                                         bl2020_mean_ACCIDENT, bl2020_mean_ACCIDENT_nf,
                                         bl2020_mean_JAM, bl2020_mean_JAM_nf)

co_NA_count %>% filter(CBSA.Code == '47900')

ggplot(co_NA_count, aes(x = NA_crash_threshold, y = NA_crash_nonthreshold)) +
  geom_count() +
  scale_size_area() +
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


# DC and DE sanity checks after fixed overlap logic ----

ggplot(d_full %>% filter(state == 'DC' | state == 'DE'), 
       aes(x = date, y = count)) +
  geom_line() +
  facet_wrap(~fips + alert_type, scales = 'free_y') +
  ggtitle('DC and DE by alert types \n Full data set')


# National-level number check ----

# Get national-level baseline total. Same for every week, but adding week variable for merging
d <- d %>%
  ungroup() %>%
  mutate(week = lubridate::week(lubridate::ymd(date))) 

nat_bl <- d %>%
  group_by(date, week) %>%
  summarize(bl2020_crash_tot = sum(count_ACCIDENT, na.rm = T),
            bl2020_weh_tot = sum(count_WEATHERHAZARD, na.rm = T),
            bl2020_jam_tot = sum(count_JAM, na.rm = T),
            bl2020_Total = sum(bl2020_crash_tot, bl2020_jam_tot, bl2020_weh_tot)
            )

# Join to national
nw <- left_join(nw, nat_bl, by = 'week')

nw_sum <- nw %>% 
  group_by(week) %>%
  summarize(Total_Waze_20 = sum(weeksum20_ACCIDENT, weeksum20_JAM, weeksum20_WEATHERHAZARD, na.rm = T),
            Total_Waze_BL = sum(bl2020_crash_tot, bl2020_weh_tot, bl2020_jam_tot, na.rm = T),
            pct_ch = ( Total_Waze_20 - Total_Waze_BL ) / Total_Waze_BL)

ggplot(nw_sum, aes(x = week, y = Total_Waze_20)) + geom_line()

# How many counties are not in a CSA but do have useful data consistently? ----
# Deprecated -- filtered to MSA already
# d_notcsa <- d %>%
#   filter(!fips %in% unique(d_msa$fips))
# 
# ds <- d_notcsa %>%
#   ungroup() %>%
#   group_by(fips) %>%
#   summarize(good_crash = sum(count_ACCIDENT > 10, na.rm = T),
#             good_weh = sum(count_WEATHERHAZARD > 10, na.rm = T),
#             good_jam = sum(count_JAM > 10, na.rm = T),
#             n_days = n(),
#             enough_good_weh = (good_weh / n_days) > .2,    
#             enough_good_crash = (good_crash / n_days) > .2, 
#             enough_good_jam = (good_jam / n_days) > .2,  
#             mostly_good = enough_good_crash & enough_good_weh & enough_good_jam
#             )
# 
# 
# table(ds$enough_good_crash) # 1
# table(ds$enough_good_jam) # 6
# table(ds$enough_good_weh) # 268
# 
# ds %>%
#   group_by(enough_good_crash, enough_good_weh, enough_good_jam) %>%
#   summarize(n())

# Riverside county FIPS
d_ex <- d_full %>% 
  filter(fips == '06065')

ggplot(d_ex, aes(x = date, y = count)) + 
  geom_line() + 
  facet_wrap(~alert_type, scales = 'free_y')

# Polk county FIPS GA, a CBSA / Micro. Insufficient data.

d_ex <- d_full %>% 
  filter(fips == '13233')

ggplot(d_ex, aes(x = date, y = count)) + 
  geom_line() + 
  facet_wrap(~alert_type, scales = 'free_y')


# Floyd county GA, a CBSA / Metro. Still insufficient in this case
d_ex <- d_full %>% 
  filter(fips == '13115')

ggplot(d_ex, aes(x = date, y = count)) + 
  geom_line() + 
  facet_wrap(~alert_type, scales = 'free_y')


# How many counties are in Metro Statistical Areas?
# Deprecated -- d_msa already filtered to Metropolitan Statistical Areas

dx <- d_msa[!duplicated(d_msa$CBSA.Code),]

# table(dx$Metropolitan.Micropolitan.Statistical.Area == 'Metropolitan Statistical Area')
# 
# dxx <- dx[dx$Metropolitan.Micropolitan.Statistical.Area == 'Metropolitan Statistical Area',]
# 
# length(unique(dxx$CBSA.Title))

# A PR county -- all blank

d_ex <- d_full %>% 
  filter(fips == '72111')

ggplot(d_ex, aes(x = date, y = count)) + 
  geom_line() + 
  facet_wrap(~alert_type, scales = 'free_y')


# Check North Dakota October 2020 issue ----
d_ex <- d %>% 
  filter(state == 'ND')

ggplot(d_ex, aes(x = date, y = count_ACCIDENT+count_WEATHERHAZARD+count_JAM)) + geom_line()

ggplot(d_ex, aes(x = date)) +
  geom_line(aes(y = count_ACCIDENT), col = 'red') +
  geom_line(aes(y = count_WEATHERHAZARD), col = 'blue') +
  geom_line(aes(y = count_JAM), col = 'green') 


# Check in full
ggplot(d_full %>% filter(state == 'ND' & date > '2020-01-01'), 
       aes(x = date, y = count)) +
  geom_line() +
  facet_wrap(.~ alert_type)

ggplot(d_full %>% filter(state == 'ND' & date > '2020-10-01' & alert_type == 'WEATHERHAZARD' &
                           fips %in% c('38017', '38059')), 
       aes(x = date, y = count)) +
  geom_line()+
  facet_wrap(.~ fips)

# Look at 2019 vs 2020 for Cass County, ND
cass_19 = d_full %>% filter(state == 'ND' & 
                              date >= '2019-10-01' &
                              date <= '2019-10-30' &
                              fips %in% c('38017')) 

cass_20 = d_full %>% filter(state == 'ND' & 
                              date >= '2020-10-01' &
                              date <= '2020-10-30' &
                              fips %in% c('38017')) 

cass_19_20 <- rbind(cass_19, cass_20)

cass_19_20 = cass_19_20 %>%
  mutate(year = as.factor(year),
         day_of_month = as.numeric(format(date, '%d')))

ggplot(cass_19_20, aes(x = day_of_month, y = count, color = year)) +
  geom_line(size = 2)+
  facet_wrap(.~ alert_type) +
  ggtitle('Cass County, ND, comparing 2019 and 2020 alert counts in October')


# ND April 2021 issue ----


# Check North Dakota October 2020 issue ----
d_ex <- d %>% 
  filter(state == 'ND')

ggplot(d_ex, aes(x = date, y = count_ACCIDENT+count_WEATHERHAZARD+count_JAM)) + geom_line()

# Check in full
ggplot(d_full %>% filter(state == 'ND' & date > '2021-04-01'), 
       aes(x = date, y = count)) +
  geom_line() +
  facet_wrap(.~ alert_type)

ggplot(d_full %>% filter(state == 'ND' & date > '2021-04-01' & alert_type == 'WEATHERHAZARD' &
                           fips %in% c('38017', '38059')), 
       aes(x = date, y = count)) +
  geom_line()+
  facet_wrap(.~ fips)

# Look at 2019 vs 2021 for Cass County, ND
cass_19 = d_full %>% filter(state == 'ND' & 
                              date >= '2019-04-01' &
                              date <= '2019-04-30' &
                              fips %in% c('38017')) 

cass_21 = d_full %>% filter(state == 'ND' & 
                              date >= '2021-04-01' &
                              date <= '2021-04-30' &
                              fips %in% c('38017')) 

cass_19_21 <- rbind(cass_19, cass_21)

cass_19_21 = cass_19_21 %>%
  mutate(year = as.factor(year),
         day_of_month = as.numeric(format(date, '%d')))

ggplot(cass_19_21, aes(x = day_of_month, y = count, color = year)) +
  geom_line(size = 2)+
  facet_wrap(.~ alert_type) +
  ggtitle('Cass County, ND, comparing 2019 and 2021 alert counts in April')

