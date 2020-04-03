# Plot and summarize expeded counts from modeling work 

# setup ----
library(tidyverse)

load('Data/Waze_Covid_joined.RData')
output.loc = 'Output'
load(file.path(output.loc, 'Waze_2020_Predicted_Observed.RData')
