# add week of year to msa day

library(tidyverse)
library(readr)

code_loc = '~/git/covid_waze'
local_dir = file.path(code_loc, 'Output', Sys.Date())


# Daily MSA
d_MSA_day <- read_csv(file.path(local_dir, 'Waze_2020_MSA_day.csv'),
                      col_types = cols(dowavg19_ACCIDENT = col_double(),
                                       dowavg19_JAM = col_double(),
                                       dowavg19_WEATHERHAZARD = col_double(),
                                       bl2020_mean_ACCIDENT = col_double(),
                                       bl2020_mean_JAM = col_double(),
                                       bl2020_mean_WEATHERHAZARD = col_double()
                      ))



d_MSA_day <- d_MSA_day %>%
  mutate(week = lubridate::epiweek(d_MSA_day$date))

# Validate
d_MSA_day %>%
  filter(year == '2020' & week > 51 & grepl('Detroit', CBSA.Title)) %>%
  select(CBSA.Title, date, year, count_ACCIDENT, week)

d_MSA_day %>%
  filter(year == '2021' & grepl('Detroit', CBSA.Title)) %>%
  select(CBSA.Title, date, year, count_ACCIDENT, week)

write.csv(d_MSA_day, file = file.path(local_dir, 'Waze_MSA_day_w_week.csv'), row.names = F)
