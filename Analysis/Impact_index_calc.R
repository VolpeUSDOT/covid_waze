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

WITH_RF = FALSE # To skip the random forest output stuff

#load('Data/Waze_Covid_joined.RData')
load('Data/Waze_Covid_joined_2020-04-17.RData')
# load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

#  Generate expected values by county/day for 2020 based on 2018 and 2019 data ----
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

write.csv(waze_avg, file = file.path(output.loc, "waze_avg_counts.csv"), row.names=FALSE)

# 2020 baseline values - county level averages by day of week for Jan 5th-Feb 8th, 2020 ----
df_bl2020 <- filter(df, year == "2020" & date >= "2020-01-05" & date <= "2020-02-08" )
waze_bl2020 <- df_bl2020 %>%
  group_by(alert_type, fips, state, county, day_week) %>%
  summarise(bl2020_mean = round(mean(count, na.rm=TRUE),1), 
            bl2020_median = round(median(count, na.rm=TRUE),1), 
            bl2020_sd = round(sd(count, na.rm=TRUE),1), 
            bl2020_n = n())

write.csv(waze_bl2020, file = file.path(output.loc, "waze_baseline_2020.csv"), row.names=FALSE)

# 2020 county and state baseline totals for Jan 5th-Feb 8th, 2020 ----
waze_county_bl2020 <- waze_bl2020 %>%
  group_by(alert_type, fips, state, county) %>%
  summarise(bl2020_cty_sum = round(sum(bl2020_mean, na.rm=TRUE),1), 
            bl2020_cty_n = n())
write.csv(waze_county_bl2020, file = file.path(output.loc, "waze_county_bl2020.csv"), row.names=FALSE)

waze_state_bl2020 <- waze_bl2020 %>%
  group_by(alert_type, state) %>%
  summarise(bl2020_st_sum = round(sum(bl2020_mean, na.rm=TRUE),1), 
            bl2020_st_n = n())
write.csv(waze_state_bl2020, file = file.path(output.loc, "waze_state_bl2020.csv"), row.names=FALSE)

wazeall_county_bl2020 <- waze_bl2020 %>%
  group_by(fips, state, county) %>%
  summarise(bl2020_cty_sum_all = round(sum(bl2020_mean, na.rm=TRUE),1), 
            bl2020_cty_n_all = n())
write.csv(wazeall_county_bl2020, file = file.path(output.loc, "wazeall_county_bl2020.csv"), row.names=FALSE)

wazeall_state_bl2020 <- waze_bl2020 %>%
  group_by(state) %>%
  summarise(bl2020_st_sum_all = round(sum(bl2020_mean, na.rm=TRUE),1), 
            bl2020_st_n_all = n())
write.csv(wazeall_state_bl2020, file = file.path(output.loc, "wazeall_state_bl2020.csv"), row.names=FALSE)


# Calculating impact for modeled results----

# 1. Collapse to a single day per county
# 2. Pivot to wide the counts of observed alerts and predicted counts of alerts
# 3. For each alert type, calculate percent change from predicted to observed 
# 4. Average the three alert types
if(WITH_RF){
  # compiled_pred_s <- compiled_pred %>% filter(state == 'RI') # Smaller sample set for testing code
  
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
  
  # This is the average decrease in activity. 
  impact_index = ( rowSums(compiled_pred_w[,c('impact_crash',
                                            'impact_weh',
                                            'impact_jam')]) / 3 )
  
  compiled_pred_w <- data.frame(compiled_pred_w, impact_index)
  
  # Sanity check: are our cases and deaths variables still correct by FIPS and date?
  compiled_pred_w %>% filter(fips == '56039' & date == '2020-03-30') %>% select(date, state, county, cases, deaths, fips)
  df %>% filter(fips == '56039' & yearday == '2020-03-30') %>% select(date, state, county, cases, deaths, fips)
  
  compiled_pred_w %>% filter(fips == '55079' & date == '2020-03-30') %>% select(date, state, county, cases, deaths, fips)
  df %>% filter(fips == '55079' & yearday == '2020-03-30') %>% select(date, state, county, cases, deaths, fips)
  # All good.
}
# Merge historical means by month and day of week (2017-2019) to give different options for calculating response indices----

waze_avg$day_week_ch <- as.character(waze_avg$day_week)
if(WITH_RF) { compiled_pred_w$month_ch <- as.character(compiled_pred_w$month) }
waze_avg$month_int <- as.integer(waze_avg$month)

#make historical data wide
waze_avg_w <- waze_avg %>% 
  pivot_wider(names_from = alert_type,
              values_from = c(hist_mean, hist_median, hist_sd))

#join historical mean data to waze indices file 
# Update this when re-run compiled_pred RF. For now just use waze_avg_w as _indicies file.
if(WITH_RF){
  Waze2020_indices <- compiled_pred_w %>%
    left_join(waze_avg_w,
              by = c('state' = 'state', 'county'='county', 'fips'='fips', 'month' = 'month_int', 'day_week'='day_week_ch'))
} else {
# get counts by day and county from df
  df_w <- df %>% 
    filter(year == "2020") %>%
    pivot_wider(names_from = alert_type,
                values_from = c(count))
  
  df_w <- df_w %>% 
    rename(count_ACCIDENT = ACCIDENT,
           count_WEATHERHAZARD = WEATHERHAZARD,
           count_JAM = JAM)
  # Clean up to free up RAM
  rm(df, df_bl2020, df_no2020,
     wazeall_county_bl2020, wazeall_state_bl2020); gc()
  
  Waze2020_indices = df_w %>% 
    left_join(waze_avg_w,
              by = c('state' = 'state', 'county'='county', 'fips'='fips', 'month' = 'month', 'day_week'='day_week'))

}

#Merge baseline 2020 data by day of week to give different options for calculating response indices----
waze_bl2020$day_week_ch <- as.character(waze_bl2020$day_week)

#make baseline 2020 data wide
waze_bl2020_w <- waze_bl2020 %>% 
  pivot_wider(names_from = alert_type,
              values_from = c(bl2020_mean, bl2020_median, bl2020_sd))

#join baseline data to waze indices file 
Waze2020_indices <- Waze2020_indices %>% 
  left_join(waze_bl2020_w, by = c('state' = 'state', 'county'='county', 'fips'='fips', 'day_week'='day_week'))

# Trim to today ---
Waze2020_indices = Waze2020_indices %>% filter(date <= Sys.Date())

# Save -----

if(WITH_RF) {
    save_list = c('compiled_pred', 'compiled_pred_w', 'waze_avg', 'waze_bl2020', 'Waze2020_indices')
    write.csv(compiled_pred_w, file = file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.csv'), row.names = F)
  } else {
    save_list = c('waze_avg', 'waze_bl2020', 'Waze2020_indices')
  }

save(file = file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'),
     list = save_list)

write.csv(Waze2020_indices, file = file.path(output.loc, 'Waze_2020_Indices.csv'), row.names = F)

