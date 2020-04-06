# Calculate a measure of impact from difference between observed and expected counts
# Plot and summarize this impact index
# Reference covid-19 cases and disaster declarations as interpreting factors in the change in impact index


# setup ----
library(tidyverse)

output.loc = 'Output'

load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed.RData'))

# Calculating impact ----

# 1. Collapse to a single day per county
# 2. Pivot to wide the counts of observed alerts and predicted counts of alerts
# 3. For each alert type, calculate percent change from predicted to observed 
# 4. Average the three alert types

compiled_pred_s <- compiled_pred %>% filter(state == 'RI')

compiled_pred_w <- compiled_pred %>% 
  pivot_wider(names_from = alert_type,
              values_from = c(count, pred_count))

# Impact: percent decrease in activity compared to expected (pred_count)

compiled_pred_w <- compiled_pred_w %>%
  mutate(impact_crash = (( pred_count_ACCIDENT - count_ACCIDENT) / pred_count_ACCIDENT ),
         impact_weh =   (( pred_count_WEATHERHAZARD - count_WEATHERHAZARD) / pred_count_WEATHERHAZARD ),
         impact_jam =   (( pred_count_JAM - count_JAM) / pred_count_JAM )
         )

# Outlier filtering ----
# Within county, omit any values for observed count_* which are > 2 s.d. above mean of count_* in that county in 2020
# drop in Box with _filtered.csv for Michelle to use

# Google style ----
# just use median counts for previous 5 weeks from Jan 3 - Feb 6
# Michelle will do this in Tableau


# This is the average decrease in activity. 
# Multiply by -1 to make the sign logical:
# A negative value indicates a decrease in activty by that proportion
# -1 = 100% decrease in activity compared to expected
#  1 = 100% increase in activity compared to expected
impact_index = -1 * ( rowSums(compiled_pred_w[,c('impact_crash',
                                          'impact_weh',
                                          'impact_jam')]) / 3 )

compiled_pred_w <- data.frame(compiled_pred_w, impact_index)



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
