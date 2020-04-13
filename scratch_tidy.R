# fitler 2020
library(tidyverse)
df <- readr::read_csv('Output/Waze_2020_Index_cleaned.csv',
                      col_types = cols(
                        .default = col_double(),
                        fips = col_character(),
                        state = col_character(),
                        county = col_character(),
                        cases = col_integer(),
                        deaths = col_integer(),
                        date = col_date(format = ""),
                        month = col_character(),
                        date_weekday = col_date(format = ""),
                        day_week = col_character(),
                        day_week_ch.x = col_character(),
                        day_week_ch.y = col_character()
                      ))

df2020 <- dplyr::filter(df, year == '2020')


df2020 = df2020 %>%
  rename(count_ACCIDENT = ACCIDENT,
         count_WEATHERHAZARD = WEATHERHAZARD,
         count_JAM = JAM)

Waze2020_indices = df2020

# Remove weather/hazard outliers (~6 values from Jan 1 2020 in NY/CT counties - error with data)
Waze2020_indices$count_WEATHERHAZARD[which(Waze2020_indices$count_WEATHERHAZARD>10000)] <- NA

#plot(Waze2020_indices$count_WEATHERHAZARD)
summary(Waze2020_indices$count_WEATHERHAZARD) #range now to ~4800 

# rejoin cases/deaths


write.csv(df2020, file = 'Output/Waze_2020_Index_Cleaned.csv', row.names = F)
