# Assess and remove outliers and threshold predicted values for Waze 2020 alert types to clean up index values

# setup ----
library(tidyverse)
library(egg)
library(lubridate)

output.loc = 'Output'

#load('Data/Waze_Covid_joined.RData')
load(file.path(output.loc, 'Waze_2020_Predicted_Observed_Index.RData'))

# Apply filters to large observed Waze alert outliers----
#plot(Waze2020_indices$count_ACCIDENT) #no clear outliers
summary(Waze2020_indices$count_ACCIDENT) #max ~1,114

#plot(Waze2020_indices$count_JAM) #no clear outliers
summary(Waze2020_indices$count_JAM) #max ~18,360

#plot(Waze2020_indices$count_WEATHERHAZARD) #some huge outliers
summary(Waze2020_indices$count_WEATHERHAZARD)

# Remove weather/hazard outliers (~6 values from Jan 1 2020 in NY/CT counties - error with data)
Waze2020_indices$count_WEATHERHAZARD[which(Waze2020_indices$count_WEATHERHAZARD > 10000)] <- NA

# plot(Waze2020_indices$count_WEATHERHAZARD)
summary(Waze2020_indices$count_WEATHERHAZARD) #range now to ~95297 

# Look at large predicted Waze alert outliers----
#plot(Waze2020_indices$pred_count_ACCIDENT) #some large values - look at source
Waze2020_indices[which(Waze2020_indices$pred_count_ACCIDENT>600),] #Los Angeles - don't apply filter
summary(Waze2020_indices$pred_count_ACCIDENT) #Max ~1600

#plot(Waze2020_indices$pred_count_JAM) #no clear outliers
Waze2020_indices[which(Waze2020_indices$pred_count_JAM>10000),] #Los Angeles - don't apply filter
summary(Waze2020_indices$pred_count_JAM) #Max ~19,000

#plot(Waze2020_indices$pred_count_WEATHERHAZARD) #
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
Waze2020_indices <- Waze2020_indices %>%
  mutate(impact_crash_th = (( count_ACCIDENT - pred_count_ACCIDENT) / pred_count_ACCIDENT ),
         impact_weh_th =   (( count_WEATHERHAZARD - pred_count_WEATHERHAZARD) / pred_count_WEATHERHAZARD ),
         impact_jam_th =   (( count_JAM - pred_count_JAM) / pred_count_JAM )
  )

#replace Inf with NA (from dividing by zero)
Waze2020_indices$impact_crash_th[is.nan(Waze2020_indices$impact_crash_th) == TRUE] <- NA
Waze2020_indices$impact_weh_th[is.nan(Waze2020_indices$impact_weh_th) == TRUE] <- NA
Waze2020_indices$impact_jam_th[is.nan(Waze2020_indices$impact_jam_th) == TRUE] <- NA

Waze2020_indices$impact_crash_th[which(Waze2020_indices$impact_crash_th==Inf)] <- NA
Waze2020_indices$impact_weh_th[which(Waze2020_indices$impact_weh_th==Inf)] <- NA
Waze2020_indices$impact_jam_th[which(Waze2020_indices$impact_jam_th==Inf)] <- NA

summary(Waze2020_indices$impact_crash_th)
summary(Waze2020_indices$impact_weh_th)
summary(Waze2020_indices$impact_jam_th)

# This is the average decrease in activity.
# Use total instead of average? Or weighted average?
impact_index_th = ( rowSums(Waze2020_indices[,c('impact_crash_th',
                                            'impact_weh_th',
                                            'impact_jam_th')]) / 3 )

Waze2020_indices <- data.frame(Waze2020_indices, impact_index_th)



#Round predicted values to whole numbers to calculate impact factors----
#Use this or the threshold option above

#re-calculate impact factors with rounded values----
Waze2020_indices <- Waze2020_indices %>%
  mutate(impact_crash_rnd = (( count_ACCIDENT - round(pred_count_ACCIDENT,0)) / round(pred_count_ACCIDENT,0)),
         impact_weh_rnd =   (( count_WEATHERHAZARD - round(pred_count_WEATHERHAZARD,0)) / round(pred_count_WEATHERHAZARD,0)),
         impact_jam_rnd =   (( count_JAM - round(pred_count_JAM,0)) / round(pred_count_JAM,))
  )

#replace NaN with NA
Waze2020_indices$impact_crash_rnd[is.nan(Waze2020_indices$impact_crash_rnd) == TRUE] <- NA
Waze2020_indices$impact_weh_rnd[is.nan(Waze2020_indices$impact_weh_rnd) == TRUE] <- NA
Waze2020_indices$impact_jam_rnd[is.nan(Waze2020_indices$impact_jam_rnd) == TRUE] <- NA

#Replace Inf with NA
Waze2020_indices$impact_crash_rnd[which(Waze2020_indices$impact_crash_rnd==Inf)] <- NA
Waze2020_indices$impact_weh_rnd[which(Waze2020_indices$impact_weh_rnd==Inf)] <- NA
Waze2020_indices$impact_jam_rnd[which(Waze2020_indices$impact_jam_rnd==Inf)] <- NA

summary(Waze2020_indices$impact_crash_th)
summary(Waze2020_indices$impact_weh_th)
summary(Waze2020_indices$impact_jam_th)

# This is the average decrease in activity.
# Use total instead of average? Or weighted average?
impact_index_rnd = ( rowSums(Waze2020_indices[,c('impact_crash_rnd',
                                                'impact_weh_rnd',
                                                'impact_jam_rnd')]) / 3 )

Waze2020_indices <- data.frame(Waze2020_indices, impact_index_rnd)

# Save -----
save(file = file.path(output.loc, 'Waze_2020_Index_cleaned.RData'),
     list = c('Waze2020_indices'))

write.csv(Waze2020_indices, file = file.path(output.loc, 'Waze_2020_Index_cleaned.csv'), row.names = F)


# Histograms by day of week
ggplot(Waze2020_indices) + 
  geom_histogram(aes(count_JAM)) + 
  scale_x_log10() +
  facet_wrap(~day_week)


# check CA

ca_sum <- Waze2020_indices %>% 
  filter(state == 'CA') %>% 
  group_by(date) %>% 
  summarize(sum(count_ACCIDENT, na.rm=T),
            sum(hist_mean_ACCIDENT, na.rm=T))
