# Assess and remove outliers and threshold predicted values for Waze 2020 alert types to clean up index values

# setup ----
library(tidyverse)
library(egg)
library(lubridate)

output.loc = 'Output'

#load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

#Explore patterns
plot(Waze2020_indices$pred_count_JAM ~ Waze2020_indices$hist_mean_JAM)

#look at high predicted values
jh <- Waze2020_indices[which(Waze2020_indices$pred_count_JAM>10000),]
#These are LA - missing historical and 2020 baseline data, but have model results. 
#Look at plot again when CA data are fixed - predicted values are truncated at 5,000, except for the 86 values above 10,000 that are all LA.
#Explore patterns
jam_filt <- Waze2020_indices[which(Waze2020_indices$pred_count_JAM<10000),]
plot(jam_filt$pred_count_JAM ~ jam_filt$hist_mean_JAM)

jam_filt2 <- Waze2020_indices[which(Waze2020_indices$pred_count_JAM>10000 & Waze2020_indices$hist_mean_JAM<1),]
plot(jam_filt$pred_count_JAM ~ jam_filt$hist_mean_JAM)
#Again, stipe of high predicted values and zero historical is LA

# Histograms by day of week
ggplot(Waze2020_indices) + 
  geom_histogram(aes(count_JAM)) + 
  scale_x_log10() +
  facet_wrap(~day_week)

#Ideas to test----
# Note: biggest impacts are when predicted values are small, so you're dividing by a small value (e.g., 0.02) 
# When the observed value is 0, and predicted value is greater than 0 you get an impact of -1
# Need to threshold predicted values with model tuning? E.g., predicted jams and weh of < 0.5 are assigned zero.
# Use same threshold for crashes as for Waze analysis (values below ~0.25 are assigned zero)

# Outlier filtering ----
# Within county, omit any values for observed count_* which are > 2 s.d. above mean of count_* in that county in 2020
# drop in Box with _filtered.csv for Michelle to use

# Google style ----
# Use median counts for previous 5 weeks from Jan 3 - Feb 6
# 2020 Baseline values are now included in data file 

