# Join data sources and generate expected counts of alerts for 2020.

# Data 
# Currently all stored locally, will reference shared drives when that is working again
# Waze: Compiled counts of Waze alerts by type, by county, by day and hour (currently UTC), since inception of SDC dataset
# covid: NYT counts of cases by county and day
# Declarations: TBD

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
load('Data/Compiled_county_counts.RData') # Get this from SDC export_requests

waze <- compiled_counts # rename for ease 

# Covid
# Refresh data source. Assumes you have cloned nytimes/covid-19-data
# Check this, seems to work
system(paste0('cd ', git_dir, '/covid-19-data/', '; git pull'))

covid <- read.csv(file.path(git_dir, 'covid-19-data', 'us-counties.csv'), stringsAsFactors = F)

# Join ----

covid$fips <- formatC(covid$fips, width = 5, flag = "0")

# Rename and aggregate to day
waze <- waze %>%
  mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
  group_by(fips, yearday, alert_type) %>%
  summarize(count = sum(count))

# Fill in all county and state names using usmap package
all_fips <- unique(df$fips)
all_fips_table <- fips_info(all_fips)

# Not all counties are represented for all days. Need to expand grid to include all US counties, to accurately represent 0's
all_dates <- unique(waze$yearday[!is.na(waze$yearday)])
all_types <- unique(waze$alert_type[!is.na(waze$alert_type)])
all_grid <- expand.grid(fips = all_fips_table$fips, yearday = all_dates, alert_type = all_types)

waze_df <- left_join(all_grid, waze, by = c('fips', 'yearday', 'alert_type'))
waze_df[is.na(waze_df)] = 0

# Use left join instead of full join here. This limits the data to when we have Waze alert data, which will lag behind NYT covid data currently by several days. 
# Note also that covid cases data will be repeated for each fips x date x alert_type combination. Could instead  make waze_df wide first, then join.
df <- left_join(waze_df, covid,
                by = c('yearday' = 'date',
                       'fips' = 'fips'))

# Needs to have same number of rows as all_grid above
stopifnot(identical(nrow(df), nrow(all_grid)))

df <- df %>%
  select(-county, -state) %>%
  left_join(all_fips_table %>% select(-full), by = 'fips')

df <- df %>%
  rename(state = abbr)

# Fill NA with zeros
df[is.na(df)] = 0

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
