# From the data joined in Join_Data.R, train a model to generate expected counts of Waze alerts by alert type and county, at a daily level.
# Train the models on all available data from 2017 - 2019
# Generate expected counts for 2020, daily and by county
# Compare observed to expected to get an index of how affected a given county is by day.
# Output in a format which can be displayed in Tableau

# setup ----
library(tidyverse)
library(lmer) # for glmer models of Poisson counts, with county as random effect
library(randomForest) # for random forest models of counts for comparison (maybe)

load('Data/Waze_Covid_joined.RData')

