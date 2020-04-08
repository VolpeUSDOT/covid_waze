# Generate historical mean data for Tableau Dashboard of Waze / Covid case data 
# Mean historical (2017-2019) Waze counts and Baseline 2020 values
# Calculate a measure of impact from difference between observed and expected (modeled) counts
# Plot and summarize this impact index
# Reference covid-19 cases and disaster declarations as interpreting factors in the change in impact index

# setup ----
library(tidyverse)
library(usmap)
library(egg)
library(lubridate)

output.loc = 'Output'

load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

#  Generate expected values by county/day for 2020 based on 2018 and 2019 data----
#Use day of week (not day of year) to capture weekend/weekday patterns 
df$date_weekday <- as.Date(df$date)
df$day_week <- wday(ymd(df$date_weekday),label = TRUE, abbr = FALSE)

#2017-2019 mean values - county level averages by month and dow
df_no2020 <- filter(df, year != "2020")
waze_avg <- df_no2020 %>%
  group_by(alert_type, fips, state, county, month, day_week) %>%
  summarise(hist_mean = round(mean(count, na.rm=TRUE),1),
            hist_median = round(median(count, na.rm=TRUE),1),
            hist_sd = round(sd(count, na.rm=TRUE),1),
            hist_n = n())

write.csv(waze_avg, file=file.path(output.loc,"waze_avg_counts.csv"), row.names=FALSE)

#2020 baseline values - county level averages by day of week for Jan 5th-Feb 8th, 2020----
df_bl2020 <- filter(df, year == "2020" & date >= "2020-01-05" & date <= "2020-02-08" )
waze_bl2020 <- df_bl2020 %>%
  group_by(alert_type, fips, state, county, day_week) %>%
  summarise(bl2020_mean = round(mean(count, na.rm=TRUE),1), 
            bl2020_median = round(median(count, na.rm=TRUE),1), 
            bl2020_sd = round(sd(count, na.rm=TRUE),1), 
            bl2020_n = n())

write.csv(waze_bl2020, file=file.path(output.loc,"/waze_baseline_2020.csv"), row.names=FALSE)

# Calculating impact for modeled results----

# 1. Collapse to a single day per county
# 2. Pivot to wide the counts of observed alerts and predicted counts of alerts
# 3. For each alert type, calculate percent change from predicted to observed 
# 4. Average the three alert types

compiled_pred_s <- compiled_pred %>% filter(state == 'RI')

compiled_pred_w <- compiled_pred %>% 
  pivot_wider(names_from = alert_type,
              values_from = c(count, pred_count))

# Impact: percent decrease in activity compared to expected (pred_count)
# Updated calculation to (observed-predicted)/(predicted) to get sign right (negative is lower than predicted value)
compiled_pred_w <- compiled_pred_w %>%
  mutate(impact_crash = (( count_ACCIDENT - pred_count_ACCIDENT) / pred_count_ACCIDENT ),
         impact_weh =   (( count_WEATHERHAZARD - pred_count_WEATHERHAZARD) / pred_count_WEATHERHAZARD ),
         impact_jam =   (( count_JAM - pred_count_JAM) / pred_count_JAM )
         )

#Ideas to test----
# Note: biggest impacts are when predicted values are small, so you're dividing by a small value (e.g., 0.02) 
# When the observed value is 0, and predicted value is greater than 0 you get an impact of -1
# Need to threshold predicted values with model tuning? E.g., predicted jams and weh of < 0.5 are assigned zero.
# Use same threshold for crashes as for Waze analysis (values below ~0.25 are assigned zero)

# Outlier filtering ----
# Within county, omit any values for observed count_* which are > 2 s.d. above mean of count_* in that county in 2020
# drop in Box with _filtered.csv for Michelle to use

# Google style ----
# just use median counts for previous 5 weeks from Jan 3 - Feb 6
# Michelle will do this in Tableau


# This is the average decrease in activity. 
impact_index = ( rowSums(compiled_pred_w[,c('impact_crash',
                                          'impact_weh',
                                          'impact_jam')]) / 3 )

compiled_pred_w <- data.frame(compiled_pred_w, impact_index)

# Not working yet -----

#Merge historical means by month and day of week (2017-2019) and 2020 baseline values to give different options for calculating response indices.
waze_avg$day_week_ch <- as.character(waze_avg$day_week)
compiled_pred_w$month_ch <- as.character(compiled_pred_w$month)

Waze2020_indices <- compiled_pred_w %>% 
                    left_join(waze_avg, by = 'fips')


df <- df %>%
  select(-county, -state) %>%
  left_join(all_fips_table %>% select(-full), by = 'fips')




# Save-----
save(file = file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'),
     list = c('compiled_pred', 'compiled_pred_w'))

write.csv(compiled_pred_w, file = file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.csv'), row.names = F)





# old 
# impact_df <- compiled_pred %>%
#   group_by(date, fips, alert_type) %>%
#   summarize(impact_alert = (pred_count - count ) / pred_count) %>%
#   ungroup() %>%
#   group_by(date, fips) %>%
#   summarize(impact = mean(impact_alert, na.rm = T))
# 
# to_join <- compiled_pred %>% 
#   select(date, fips, state, county, cases, deaths, 
#          year, month, dow, day_week, weekend, week_year) %>%
#   distinct()
#   
# impact_df <- impact_df %>%
#   left_join(to_join, by = c('date', 'fips'))
# 
# impact_df_summary <- impact_df %>%
#   group_by(month, state) %>%
#   summarize(mean_impact = mean(impact, na.rm=T))
#   
