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

df <- left_join(waze, covid,
                by = c('yearday' = 'date',
                       'fips' = 'fips'))

# Fill in all county and state names using usmap package

all_fips <- unique(df$fips)
all_fips_table <- fips_info(all_fips)

df <- df %>%
  select(-county, -state) %>%
  left_join(all_fips_table %>% select(-full), by = 'fips')

df <- df %>%
  rename(state = abbr)

# Fill NA with zeros
df[is.na(df)] = 0

# Save ----

save(df, file = 'Data/Waze_Covid_joined.RData')
write.csv(df, file = 'Data/Waze_Covid_joined.csv', row.names = F)
