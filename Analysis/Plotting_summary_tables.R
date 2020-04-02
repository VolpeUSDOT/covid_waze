# Exploratory data analysis of joined Waze / Covid case data 

# setup ----
library(tidyverse)
library(egg)

load('Data/Waze_Covid_joined.RData')
output.loc = 'Output'

# Histograms by alert type
ggplot(df) + 
  geom_histogram(aes(count)) + 
  scale_x_log10() +
  facet_wrap(~alert_type)

# time series for some selected counties
# !!! NOTE: Waze data lag behind covid case data. There are a handful of alerts which are for recent dates, but the bulk of the data is not present for the most recent days. 
# Therefore, for these time series plots, I am currently truncating the dataframe by 7 days.
df_all <- df
df <- df %>%
  filter(date <= Sys.Date()-7)

select_counties <- c('Middlesex County', 'Cook County', 'Snohomish County')

df_s <- df %>% filter(county %in% select_counties & 
                        state %in% c('WA', 'MA', 'IL'))

ggplot(df_s, aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~county, 
             nrow = length(select_counties),
             scales = 'free_y') +
  ggtitle('Time series of Waze alert counts for three example counties')

ggsave(file = file.path(output.loc, 'Example_full_time_series_plots.jpeg'),
       width = 15, height = 15)


# Crash reports only
ggplot(df_s %>% filter(alert_type == 'ACCIDENT'), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~county, 
             nrow = length(select_counties),
             scales = 'free_y') +
  ggtitle('Time series of Waze crash counts for three example counties')

ggsave(file = file.path(output.loc, 'Example_full_time_series_plots_crash.jpeg'),
       width = 15, height = 15)

# Same, but now compare 2018 - 2020 sepearately for these counties
gp1 <- ggplot(df_s %>% filter(year == '2018' & month %in% c('01', '02', '03')), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  ylim(c(0, 6000)) +
  facet_wrap(~county) +
  ggtitle('2018')


gp2 <- ggplot(df_s %>% filter(year == '2019' & month %in% c('01', '02', '03')), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  ylim(c(0, 6000)) +
  facet_wrap(~county) +
  ggtitle('2019')

gp3 <- ggplot(df_s %>% filter(year == '2020' & month %in% c('01', '02', '03')), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  ylim(c(0, 6000)) +
  facet_wrap(~county) +
  ggtitle('2020')

gpall <- ggarrange(gp1, gp2, gp3,
                   ncol = 1,
                   heights = c(1, 1, 1))

ggsave(gpall, 
       file = file.path(output.loc, 'Example_compare_time_series_plots.jpeg'),
       width = 10, height = 12)

# Add covid cases for 2020 plot

gp4 <- ggplot(df_s %>% filter(year == '2020' & month %in% c('01', '02', '03')), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
#  ylim(c(0, 6000)) +
  facet_wrap(~county, nrow = length(select_counties), scale = 'free_y') +
  ggtitle('2020 \n covid-19 case counts in black')  +
  geom_line(aes(x = date, y = cases),
            size = 2,
            color = 'black')

ggsave(gp4, 
       file = file.path(output.loc, 'Example_compare_time_series_plots_with_cases.jpeg'),
       width = 8, height = 8)
