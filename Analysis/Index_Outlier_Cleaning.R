# Assess and remove outliers and threshold predicted values for Waze 2020 alert types to clean up index values

# setup ----
library(tidyverse)
library(egg)
library(lubridate)

output.loc = 'Output'

#load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

# Apply filters to large observed Waze alert outliers----
plot(Waze2020_indices$count_ACCIDENT) #no clear outliers
summary(Waze2020_indices$count_ACCIDENT) #max ~850

plot(Waze2020_indices$count_JAM) #no clear outliers
summary(Waze2020_indices$count_JAM) #max ~8,000

plot(Waze2020_indices$count_WEATHERHAZARD) #some huge outliers
summary(Waze2020_indices$count_WEATHERHAZARD)

# Remove weather/hazard outliers (~6 values from Jan 1 2020 in NY/CT counties - error with data)
Waze2020_indices$count_WEATHERHAZARD[which(Waze2020_indices$count_WEATHERHAZARD>10000)] <- NA
plot(Waze2020_indices$count_WEATHERHAZARD)
summary(Waze2020_indices$count_WEATHERHAZARD) #range now to ~4800 

# Look at large predicted Waze alert outliers----
plot(Waze2020_indices$pred_count_ACCIDENT) #some large values - look at source
Waze2020_indices[which(Waze2020_indices$pred_count_ACCIDENT>600),] #Los Angeles - don't apply filter
summary(Waze2020_indices$pred_count_ACCIDENT) #Max ~1600

plot(Waze2020_indices$pred_count_JAM) #no clear outliers
Waze2020_indices[which(Waze2020_indices$pred_count_JAM>10000),] #Los Angeles - don't apply filter
summary(Waze2020_indices$pred_count_JAM) #Max ~19,000

plot(Waze2020_indices$pred_count_WEATHERHAZARD) #
Waze2020_indices[which(Waze2020_indices$pred_count_WEATHERHAZARD>3000),] #Los Angeles - don't apply filter
summary(Waze2020_indices$pred_count_WEATHERHAZARD) #Max ~6700


#Explore patterns for predicted values vs historical means----
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
#Again, stripe of high predicted values and zero historical is LA, should be fixed with updates

#Threshold predicted values to push small numbers to zero----
#This section should be applied before the impact factors are calculated in the Impact index script
# Need to threshold predicted values: 
# Predicted jams and weh of < 0.5 are assigned zero.
# Use same threshold for crashes as for Waze SDI models (values below ~0.2 are assigned zero)

#set thresholds
pred_ACC_threshold <- which(Waze2020_indices$pred_count_ACCIDENT<0.2) # ~38,000 values
pred_JAM_threshold <- which(Waze2020_indices$pred_count_JAM<0.5) #~40,500 values
pred_WEH_threshold <- which(Waze2020_indices$pred_count_WEATHERHAZARD<0.5) #33 values

#update values below threshold to zero
Waze2020_indices$pred_count_ACCIDENT[pred_ACC_threshold] <- 0
Waze2020_indices$pred_count_JAM[pred_JAM_threshold] <- 0
Waze2020_indices$pred_count_WEATHERHAZARD[pred_WEH_threshold] <- 0

#re-calculate impact factors with thresholded values


# Histograms by day of week
ggplot(Waze2020_indices) + 
  geom_histogram(aes(count_JAM)) + 
  scale_x_log10() +
  facet_wrap(~day_week)

#Ideas to test----
# Note: biggest impacts are when predicted values are small, so you're dividing by a small value (e.g., 0.02) 
# When the observed value is 0, and predicted value is greater than 0 you get an impact of -1

# Outlier filtering ----
# Within county, omit any values for observed count_* which are > 2 s.d. above mean of count_* in that county in 2020
# drop in Box with _filtered.csv for Michelle to use

# Google style ----
# Use median counts for previous 5 weeks from Jan 3 - Feb 6
# 2020 Baseline values are now included in data file 

