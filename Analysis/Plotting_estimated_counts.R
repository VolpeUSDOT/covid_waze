# Plot and summarize expeded counts from modeling work 

# setup ----
library(tidyverse)
library(egg)

output.loc = 'Output'

# load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

# Plotting ----

# Look at the observed - predicted for some example counties, by month

select_counties <- c('Middlesex County', 'Cook County', 'Snohomish County')

# another set
# select_counties <- c('Wayne County', 'Jefferson Parish', 'Broward County')

compiled_pred <- compiled_pred %>%
  mutate(date = as.Date(date))

pred_s <- compiled_pred %>% filter(county %in% select_counties & 
                        state %in% c('MA', 'IL', 'WA'))
                        # state %in% c('MI', 'LA', 'FL'))


ggplot(pred_s, aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.2) +
  geom_line(aes(x = date, y = pred_count, color = alert_type)) +
  facet_wrap(~county, 
             nrow = length(select_counties),
             scales = 'free_y') +
  ggtitle('Time series of observed (point) and estimated (line) \n Waze alert counts for three example counties')

ggsave(file = file.path(output.loc, 'Example_obs_est_time_series_plots.jpeg'),
       width = 15, height = 15)

# Compare Jan, Feb, March 2020 for these counties

gp1 <- ggplot(pred_s %>% filter(month == 1), 
              aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(aes(x = date, y = pred_count, color = alert_type), size = 1) +
  #ylim(c(0, 2500)) +
  facet_wrap(~county, scales = 'free_y') +
  ggtitle('January 2020')

gp2 <- ggplot(pred_s %>% filter(month == 2), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(aes(x = date, y = pred_count, color = alert_type), size = 1) +
#  ylim(c(0, 2500)) +
  facet_wrap(~county, scales = 'free_y') +
  ggtitle('February 2020')

gp3 <- ggplot(pred_s %>% filter(month == 3), aes(x = date, y = count, color = alert_type)) +
  geom_point(alpha = 0.5, size = 2.5) +
  geom_line(aes(x = date, y = pred_count, color = alert_type), size = 1) +
  facet_wrap(~county, scales = 'free_y') +
  ggtitle('March 2020')

gpall <- ggarrange(gp1, gp2, gp3,
                   ncol = 1,
                   heights = c(1, 1, 1))

ggsave(gpall, 
       file = file.path(output.loc, 'Example_obs_est_month_time_series_plots.jpeg'),
       width = 10, height = 12)

# Plotting with impact index and covid cases ----

compiled_pred_w <- compiled_pred_w %>%
  mutate(date = as.Date(date))

select_counties <- c('Middlesex County', 'Cook County', 'Snohomish County')

pred_s <- compiled_pred_w %>% filter(county %in% select_counties & 
                                     state %in% c('MA', 'IL', 'WA'))
# state %in% c('MI', 'LA', 'FL'))

ggplot(pred_s, aes(x = date, y = impact_index)) +
  geom_line(aes(color = impact_index), size = 2) +
  geom_hline(yintercept = 0, color = 'black', lty = 2) +
  facet_wrap(~county, 
             nrow = length(select_counties),
             scales = 'free_y') +
  ylab("Proportional change in activity compared to baseline") +
  ggtitle('Impact index for three example counties')

ggsave(file = file.path(output.loc, 'Example_impact_time_series_plots.jpeg'),
       width = 8, height = 8)


# another set
select_counties <- c('Wayne County', 'Jefferson Parish', 'Broward County')
pred_s <- compiled_pred_w %>% filter(county %in% select_counties & 
                                       state %in% c('MI', 'LA', 'FL'))

ggplot(pred_s, aes(x = date, y = impact_index)) +
  geom_line(aes(color = impact_index), size = 2) +
  geom_hline(yintercept = 0, color = 'black', lty = 2) +
  facet_wrap(~county, 
             nrow = length(select_counties),
             scales = 'free_y') +
  ylab("Proportional change in activity compared to baseline") +
  ggtitle('Impact index for three example counties')

ggsave(file = file.path(output.loc, 'Example_impact_time_series_plots_2.jpeg'),
       width = 8, height = 8)

# Plot impact by state over time, plus sum of covid cases over time

state_df <- compiled_pred_w %>%
  group_by(date, state) %>%
  summarize(sum_cases = sum(cases, na.rm =T),
            sum_deaths = sum(deaths, na.rm=T),
            median_impact_index = median(impact_index, na.rm = T))

gp_state <- ggplot(state_df %>% filter(state == 'NY'), 
                   aes(x = date, y = -1*median_impact_index, color = state)) +
  geom_hline(yintercept = 0, color = 'black', lty = 2) +
  geom_line(alpha = 0.5) 
