# Counts of crashes in NC by week for NHTSA presentation

# setup ----
library(tidyverse)
library(readr)

input.loc = 'Data'
output.loc = 'Output'

latest_refresh_day = max(dir('Output')[grep('2020-', dir('Output'))]) # e.g. '2020-05-06'

latest_refresh_day = '2020-06-29'

d_full <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_Covid_joined.csv'),
                   col_types = cols(cases = col_double(),
                                    deaths = col_double()))    


# Summarize for slide ----

nc_d <- d_full %>%
  filter(grepl('^37', fips) & year %in% c('2019','2020'))

# add week of year
nc_d <- nc_d %>%
  mutate(week = lubridate::epiweek(date))

week_counts <- nc_d %>%
  group_by(year, week) %>%
  filter(alert_type == 'ACCIDENT') %>%
  summarize(crash_count = sum(count, na.rm = T))

week_counts$year <- as.factor(week_counts$year)

week_counts_select <- week_counts %>% filter(week > 5 & week < 25)

ggplot(week_counts_select, aes(x = week, y = crash_count, color = year)) +
  scale_color_manual(values = c('blue4',
                                'grey80')) +
  geom_point(size = 2) +
  geom_line(size = 1.5) + 
  theme_bw() +
  theme(legend.position = 'top') +
  ylab('Number of Weekly Waze Crash Counts') +
  xlab('Week of Year') +
  ggtitle('Total Waze Crash Counts by Week') +
  geom_vline(aes(xintercept = 11), size = 2, color = 'grey50')


ggsave('NC_weekly_counts.jpeg',
       width = 8, height = 6)
