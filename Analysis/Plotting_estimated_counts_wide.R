# Plot and summarize expeded counts from modeling work 

# setup ----
library(tidyverse)
library(egg)

output.loc = 'Output'

# load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

# Put into long from wide
compiled_pred <- Waze2020_indices %>%
  pivot_longer(cols = c('count_ACCIDENT', 'count_JAM', 'count_WEATHERHAZARD'),
               names_to = 'alert_type',
               values_to = 'count')

compiled_hist <- Waze2020_indices %>%
  pivot_longer(cols = c('hist_mean_ACCIDENT', 'hist_mean_JAM', 'hist_mean_WEATHERHAZARD'),
               names_to = 'alert_type',
               values_to = 'hist_mean_count')

# Join

compiled_pred <- compiled_pred %>%
  full_join(compiled_hist)

# What is most recent full day of data? ----
daily_mean_all_alerts <- compiled_pred %>% 
                            filter(stringr::str_detect(alert_type, '^count_')) %>%
                            group_by(date) %>%
                            summarize(mean_count = mean(count, na.rm=T),
                                      median_count = median(count, na.rm=T),
                                      n_zero = sum(count == 0 | is.na(count)))

plot(daily_mean_all_alerts$mean_count)
plot(daily_mean_all_alerts$median_count)

tail(daily_mean_all_alerts$mean_count)

ggplot(daily_mean_all_alerts, aes(x = date, y = mean_count)) +
         geom_point()

ggplot(daily_mean_all_alerts, aes(x = date, y = n_zero)) +
  geom_point()


tail(compiled_pred %>% filter(stringr::str_detect(alert_type, '^count_')) %>% select(state, county, date, alert_type, count))

omit_dates = daily_mean_all_alerts %>% filter(n_zero > 9000) %>% select(date)

latest_full_day = daily_mean_all_alerts %>%
  filter(!date %in% omit_dates$date) %>%
  filter(date == max(date)) %>%
  select(date)

# limit date to only include latest full day
compiled_pred <- compiled_pred %>%
  filter(date <= latest_full_day$date)



# Plotting ----

# Look at the observed - predicted for some example counties, by month
select_counties <- c('Middlesex County', 'Cook County', 'Snohomish County')

# another set
select_counties <- c('Wayne County', 'Jefferson Parish', 'Broward County')


pred_s <- compiled_pred %>% filter(county %in% select_counties & 
                        #state %in% c('MA', 'IL', 'WA'))
                        state %in% c('MI', 'LA', 'FL'))

ggplot(pred_s, aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(x = date, y = hist_mean_count, color = alert_type)) +
  facet_wrap(~county, 
             nrow = length(select_counties),
             scales = 'free_y') +
  ggtitle('Time series of observed (point) and estimated (line) \n Waze alert counts for three example counties')

ggsave(file = file.path(output.loc, 'Example_obs_est_time_series_plots.jpeg'),
       width = 15, height = 15)

# Compare Jan, Feb, March 2020 for these counties

gp1 <- ggplot(pred_s %>% filter(month == '01'), 
              aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(aes(x = date, y = hist_mean_count, color = alert_type), size = 1) +
  #ylim(c(0, 2500)) +
  facet_wrap(~county, scales = 'free_y') +
  ggtitle('January 2020')

gp2 <- ggplot(pred_s %>% filter(month == '02'), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(aes(x = date, y = hist_mean_count, color = alert_type), size = 1) +
#  ylim(c(0, 2500)) +
  facet_wrap(~county, scales = 'free_y') +
  ggtitle('February 2020')

gp3 <- ggplot(pred_s %>% filter(month == '03'), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(aes(x = date, y = hist_mean_count, color = alert_type), size = 1) +
  facet_wrap(~county, scales = 'free_y') +
  ggtitle('March 2020')

gpall <- ggarrange(gp1, gp2, gp3,
                   ncol = 1,
                   heights = c(1, 1, 1))

ggsave(gpall, 
       file = file.path(output.loc, 'Example_obs_est_month_time_series_plots_1.jpeg'),
       width = 10, height = 12)

# 