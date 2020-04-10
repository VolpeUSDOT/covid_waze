# !!! This has been moved to SDC GitLab. Don't use this version !!!

# Join data sources and generate expected counts of alerts for 2020.

# Data 
# Currently all stored locally, will reference shared drives when that is working again
# Waze: Compiled counts of Waze alerts by type, by county, by day and hour (currently UTC), since inception of SDC dataset
# covid: NYT counts of cases by county and day
# Declarations: TBD

# Handling NA's:
# NA values may occur because no incidents happened in that hour, or because no data was flowing.
# Propose to fill 0's for these cases: at least one alert type had some record during that hour, for that county. OR at least one alert type had some record for that day, for that county.
# Otherwise, keep as NA, because represents no data flow rather than true zero

# setup ----
library(tidyverse)
library(usmap)

curr_wd <- path.expand(getwd())
git_dir <- unlist(strsplit(curr_wd, "/"))
git_dir <- paste(git_dir[1:(length(git_dir)-1)], collapse = '/')
# Assumes nytimes covid data are cloned here
# git_dir <- git_dir[length(git_dir)-1] # Alternative, finds just the name of the directory being used for all git-tracked repos. 

source('utility/get_packages.R')

# Waze
load('Data/Compiled_county_counts_2020-04-09.RData') # Get this from SDC export_requests

waze <- compiled_counts # rename for ease 

# Covid
# Refresh data source. Assumes you have cloned nytimes/covid-19-data
system(paste0('git -C ', git_dir, '/covid-19-data/ pull'))

covid <- read.csv(file.path(git_dir, 'covid-19-data', 'us-counties.csv'), stringsAsFactors = F)

# Join ----

covid$fips <- formatC(covid$fips, width = 5, flag = "0")

# Rename and aggregate to day
waze <- waze %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  group_by(fips, yearday, alert_type) %>%
  summarize(count = sum(count))

# Fill in all county and state names using usmap package
all_fips <- sort(unique(c(waze$fips, covid$fips)))
all_fips <- all_fips[!grepl('NA', all_fips)]
all_fips_table <- fips_info(all_fips)

# Not all counties are represented for all days. Need to expand grid to include all US counties, to accurately represent 0's
all_dates <- unique(waze$yearday[!is.na(waze$yearday)])
all_types <- unique(waze$alert_type[!is.na(waze$alert_type)])
all_grid <- expand.grid(fips = all_fips_table$fips, yearday = all_dates, alert_type = all_types)

waze_df <- left_join(all_grid, waze, by = c('fips', 'yearday', 'alert_type'))

# waze_df[is.na(waze_df)] = 0
# Fill zeros correctly ----
# Within a county, if any alert_type over that day is not NA, then fill the others with 0
# Otherwise, leave as NA

# Identify days where at least one value is filled in for this county.
# When true, this means we should fill 0 for any NA values on that day
not_all_na_days <- waze_df %>% 
  group_by(fips, yearday) %>%
  summarize(one_not_NA = !all(is.na(count)))

waze_df_filled <- waze_df %>%
  left_join(not_all_na_days, by = c('fips', 'yearday')) %>%
  mutate(count_filled = ifelse(is.na(count) & one_not_NA == TRUE, 0, 
                               ifelse(!is.na(count), count,
                                      NA)))

# Use left join instead of full join here. This limits the data to when we have Waze alert data, which will lag behind NYT covid data currently by several days. 
# Note also that covid cases data will be repeated for each fips x date x alert_type combination. Could instead  make waze_df wide first, then join.
df <- left_join(waze_df_filled %>% select(-one_not_NA, -count) %>% rename(count = count_filled), 
                covid,
                by = c('yearday' = 'date',
                       'fips' = 'fips'))

# Needs to have same number of rows as all_grid above
stopifnot(identical(nrow(df), nrow(all_grid)))

df <- df %>%
  select(-county, -state) %>%
  left_join(all_fips_table %>% select(-full), by = 'fips')

df <- df %>%
  rename(state = abbr)

# For covid-19 cases and deaths, fill zeros from the first date detected nationwide. Otherwise, leave as NA
first_covid_day = min(df$yearday[!is.na(df$cases)])

df_f <- df %>%
  mutate(cases_filled = ifelse(yearday >= first_covid_day & is.na(cases), 0, 
                               ifelse(!is.na(cases), cases, NA)),
         deaths_filled = ifelse(yearday >= first_covid_day & is.na(deaths), 0, 
                                ifelse(!is.na(deaths), deaths, NA))
         )
  
# head(df_f); tail(df_f) # confirm this worked as expected

df <- df_f %>%
  select(-cases, -deaths) %>%
  rename(cases = cases_filled,
         deaths = deaths_filled)

# organize date and time variables
df <- df %>%
  mutate(date = as.POSIXct(yearday, format = "%Y-%m-%d", tz = "UTC"),
         year = format(date, "%Y"),
         month = format(date, "%m"),
         dow = format(date, "%w")) # day of week as number, Sunday = 0

# Remove any NA fips
df <- df %>%
  filter(fips != 'NANA')

# Order by date, then fips, then alert type
df <- df[order(df$date, df$fips, df$alert_type),]

# create a version with road_closed; save both with and without
df_rc <- df
df <- df %>%
  filter(alert_type != 'ROAD_CLOSED')

# Save ----

save(df, file = 'Data/Waze_Covid_joined.RData')
write.csv(df, file = 'Data/Waze_Covid_joined.csv', row.names = F)

save(df_rc, file = 'Data/Waze_Covid_joined_rc.RData')
write.csv(df_rc, file = 'Data/Waze_Covid_joined_rc.csv', row.names = F)

# Copy to covid_waze/Data folder on Box when complete