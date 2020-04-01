# Exploratory data analysis of joined Waze / Covid case data 

# setup ----
library(tidyverse)

load('Data/Waze_Covid_joined.RData')

# Histograms by alert type
ggplot(df) + 
  geom_histogram(aes(count)) + 
  scale_x_log10() +
  facet_wrap(~alert_type)


