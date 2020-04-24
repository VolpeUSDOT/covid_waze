# Assess and remove outliers and threshold predicted values for Waze 2020 alert types to clean up index values

# setup ----
library(tidyverse)
library(egg)
library(lubridate)

output.loc = 'Output'

#load(file.path(output.loc, 'Waze_2020_Index_cleaned.RData'))
Waze2020_indices <- read.csv(file.path(output.loc, 'Waze_2020_Index_cleaned.csv'))

#Explore patterns for predicted values vs historical means----
#plot(Waze2020_indices$pred_count_JAM ~ Waze2020_indices$hist_mean_JAM)
#abline(a=1,b=1)

#Threshold predicted values to push small numbers to zero----
#This section should be applied before the impact factors are calculated
# Need to threshold predicted values: 
# Predicted jams and weh of < 0.5 are assigned zero.
# Use same threshold for crashes as for Waze SDI models (values below ~0.2 are assigned zero)

#set thresholds
pred_ACC_threshold <- which(Waze2020_indices$pred_count_ACCIDENT < 0.2) # ~38,000 values
pred_JAM_threshold <- which(Waze2020_indices$pred_count_JAM < 0.5) #~40,500 values
pred_WEH_threshold <- which(Waze2020_indices$pred_count_WEATHERHAZARD < 0.5) #33 values

#update values below threshold to zero
Waze2020_indices$pred_count_ACCIDENT[pred_ACC_threshold] <- 0
Waze2020_indices$pred_count_JAM[pred_JAM_threshold] <- 0
Waze2020_indices$pred_count_WEATHERHAZARD[pred_WEH_threshold] <- 0

#Apply a minimum count threshold to the predicted values---- 

#Use original column names for filtered data for Tableau, save original data as new name
#In SDC scripts, could update to save columns as different names to begin with
#use the names below for the "final" output
Waze2020_indices$pred_count_ACCIDENT_nf <- Waze2020_indices$pred_count_ACCIDENT
Waze2020_indices$pred_count_JAM_nf <- Waze2020_indices$pred_count_JAM
Waze2020_indices$pred_count_WEATHERHAZARD_nf <- Waze2020_indices$pred_count_WEATHERHAZARD
Waze2020_indices$bl2020_mean_ACCIDENT_nf <- Waze2020_indices$bl2020_mean_ACCIDENT
Waze2020_indices$bl2020_mean_WEATHERHAZARD_nf <- Waze2020_indices$bl2020_mean_WEATHERHAZARD
Waze2020_indices$bl2020_mean_JAM_nf <- Waze2020_indices$bl2020_mean_JAM


#Indicate counties/days with insufficient data to track changes
Waze2020_indices$pred_count_totalWaze = (rowSums(Waze2020_indices[,c('pred_count_ACCIDENT',
                                                 'pred_count_WEATHERHAZARD',
                                                 'pred_count_JAM')]))
dim(Waze2020_indices)
pred_min_threshold = 10
insuff_predcount_rows <- which(Waze2020_indices$pred_count_totalWaze<pred_min_threshold)
length(insuff_predcount_rows) #108,799 counties by days with less than 10 total Waze events

#For reference:
#161,308 counties by days with less than 20 total predicted Waze events 
#270,976 counties by days with less than 100 total predicted Waze events 

#Number of counties by days with sufficient data in each state 
table(Waze2020_indices$state)-table(Waze2020_indices$state[insuff_predcount_rows]) 
#zero in SD?? Check script - rows with sufficient data in SD in baseline and in dataset

#replace these with NA - insufficient data to record
Waze2020_indices$pred_count_ACCIDENT[insuff_predcount_rows] <- NA
Waze2020_indices$pred_count_WEATHERHAZARD[insuff_predcount_rows] <- NA
Waze2020_indices$pred_count_JAM[insuff_predcount_rows] <- NA

#Apply a minimum count threshold to the 2020 baseline values---- 
#Indicate counties/days with insufficienct data to track changes
Waze2020_indices$bl2020_mean_totalWaze = (rowSums(Waze2020_indices[,c('bl2020_mean_ACCIDENT',
                                                                     'bl2020_mean_WEATHERHAZARD',
                                                                     'bl2020_mean_JAM')]))
dim(Waze2020_indices)
bl2020_min_threshold = 10
insuff_bl2020_rows <- which(Waze2020_indices$bl2020_mean_totalWaze<bl2020_min_threshold)
length(insuff_bl2020_rows) #142,704 counties by days with less than 10 total Waze events

#Number of counties by days with sufficient data in each state 
table(Waze2020_indices$state)-table(Waze2020_indices$state[insuff_bl2020_rows])

#replace these with NA - insufficient data to record
Waze2020_indices$bl2020_mean_ACCIDENT[insuff_bl2020_rows] <- NA
Waze2020_indices$bl2020_mean_WEATHERHAZARD[insuff_bl2020_rows] <- NA
Waze2020_indices$bl2020_mean_JAM[insuff_bl2020_rows] <- NA

#TO DO in SDC (for discussion)
#Check code - for the predicted values threhold, I see rows with more than 10 observations in ND, but script shows zero rows with sufficient data 
#Remove all index columns to reduce size of dataset
#Confirm the threshold code is applied to push predicted values below .2 and .5 to 0
#Apply minimum observations code - removes data for many counties/times but should fix some issue with big percent changes from small values


