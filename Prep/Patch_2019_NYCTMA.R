# Patch MA/CT/NY May-June 2019 Data with May-June 2018 Data
# Need to do this for Waze_2020_MSA_day, Waze_2020_MSA_week, Waze_2020_National_day especially.


# setup ----
library(tidyverse)
library(lubridate)
library(readr)

input.loc = 'Data'
output.loc = 'Output'

latest_refresh_day = max(dir('Output')[grep('2020-', dir('Output'))]) # e.g. '2020-06-11'

d_full <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_Covid_joined.csv'),
                   col_types = cols(cases = col_double(),
                                    deaths = col_double()))    

d_full$day_week <- wday(ymd(d_full$date), label = TRUE, abbr = FALSE)
d_full$week <- week(ymd(d_full$date))

nd <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_National_day.csv'))       

md <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_MSA_day.csv'),
               col_types = cols(Metropolitan.Division.Code = col_character(),
                                Metropolitan.Division.Title = col_character(),
                                cases = col_double(),
                                deaths = col_double()))

mw <- read_csv(file.path(output.loc, latest_refresh_day, 'Waze_2020_MSA_week.csv'),
               col_types = cols(Metropolitan.Division.Code = col_character(),
                                Metropolitan.Division.Title = col_character()))          


msa <- readxl::read_xls(file.path('Data', 'list1_2020.xls'),
                        skip = 2)
names(msa) <- make.names(names(msa))
na_row_cbsa <- which(is.na(msa$CBSA.Code))
msa <- msa[1:(na_row_cbsa-1),]
msa$fips <- paste0(msa$FIPS.State.Code, msa$FIPS.County.Code)
msa_to_join <- msa %>% 
  dplyr::select(-County.County.Equivalent,
                -State.Name,
                -FIPS.State.Code,
                -FIPS.County.Code)

# For National_day ----
# create dowavg18_ACCIDENT, JAM, WEATHERHAZARD by date, week of year, and FIPS for MA/CT/NY from d_full

d18d <- d_full %>%
  filter(year == 2018 & month %in% c('05', '06') & state %in% c('CT', 'NY', 'MA')) %>%
  group_by(fips, alert_type, state, month, dow) %>%
  summarize(dowavg18 = mean(count, na.rm = T)) %>%
  ungroup() %>%
  group_by(alert_type, state, month, dow) %>%
  summarize(dowavg18 = sum(dowavg18, na.rm = T))
  
d18dw <- d18d %>%
  mutate(dowavg18 = ifelse(dowavg18 < 10, NA, dowavg18)) %>%
  pivot_wider(names_from = alert_type,
              values_from = dowavg18,
              names_prefix = 'dowavg18_')

nd_to_patch <- nd %>% filter(!state %in% c('CT','NY', 'MA'))
nd_patch <- nd %>% filter(state %in% c('CT','NY', 'MA'))

nd_patch <- nd_patch %>%
  left_join(d18dw)

nd_patch <- nd_patch %>%
  mutate(dowavg19_ACCIDENT = ifelse(is.na(dowavg18_ACCIDENT), dowavg19_ACCIDENT, dowavg18_ACCIDENT),
         dowavg19_JAM = ifelse(is.na(dowavg18_JAM), dowavg19_JAM, dowavg18_JAM),
         dowavg19_WEATHERHAZARD = ifelse(is.na(dowavg18_WEATHERHAZARD), dowavg19_WEATHERHAZARD, dowavg18_WEATHERHAZARD)) %>%
  dplyr::select(-dowavg18_ACCIDENT,
                -dowavg18_JAM,
                -dowavg18_WEATHERHAZARD)

# append to the to_patch df

nd_patched <- rbind(nd_to_patch, nd_patch)

nd_patched <- nd_patched[order(nd_patched$state, nd_patched$date),]
nd <- nd[order(nd$state, nd$date),]

# Should be 1:1, yes
plot(nd_patched$dowavg19_ACCIDENT[!nd_patched$state %in% c('CT','NY', 'MA')], 
     nd$dowavg19_ACCIDENT[!nd$state %in% c('CT','NY', 'MA')])

# Fixed for may/june
plot(nd_patched$dowavg19_ACCIDENT[nd_patched$state %in% c('CT','NY', 'MA')], 
     nd$dowavg19_ACCIDENT[nd$state %in% c('CT','NY', 'MA')])

# Write out
if(!dir.exists(file.path(output.loc, latest_refresh_day, 'patch'))){
  dir.create(file.path(output.loc, latest_refresh_day, 'patch'))
}

stopifnot(dim(nd_patched) == dim(nd))

write.csv(nd_patched, file = file.path(output.loc, latest_refresh_day, 'patch', 'Waze_2020_National_day.csv'), row.names = F)    

# For MSA_day ----

# Subset d_full to cbsa only fips
d_cbsa <- d_full[d_full$fips %in% msa$fips,]

# Join to CBSA 
d_cbsa <- d_cbsa %>%
  left_join(msa_to_join, by = 'fips')

# Make day 
d18c <- d_cbsa %>%
  filter(year == 2018 & month %in% c('05', '06') & state %in% c('CT', 'NY', 'MA')) %>%
  group_by(fips, alert_type, state, month, day_week,
           ) %>%
  summarize(dowavg18 = mean(count, na.rm = T))

d18cw <- d18c %>%
  mutate(dowavg18 = ifelse(dowavg18 < 10, NA, dowavg18)) %>%
  pivot_wider(names_from = alert_type,
              values_from = dowavg18,
              names_prefix = 'dowavg18_')

md_to_patch <- md %>% filter(!state %in% c('CT','NY', 'MA'))
md_patch <- md %>% filter(state %in% c('CT','NY', 'MA'))

md_patch <- md_patch %>%
  left_join(d18cw)

md_patch <- md_patch %>%
  mutate(dowavg19_ACCIDENT = ifelse(is.na(dowavg18_ACCIDENT), dowavg19_ACCIDENT, dowavg18_ACCIDENT),
         dowavg19_JAM = ifelse(is.na(dowavg18_JAM), dowavg19_JAM, dowavg18_JAM),
         dowavg19_WEATHERHAZARD = ifelse(is.na(dowavg18_WEATHERHAZARD), dowavg19_WEATHERHAZARD, dowavg18_WEATHERHAZARD)) %>%
  dplyr::select(-dowavg18_ACCIDENT,
                -dowavg18_JAM,
                -dowavg18_WEATHERHAZARD)

# append to the to_patch df

md_patched <- rbind(md_to_patch, md_patch)

md_patched <- md_patched[order(md_patched$state, md_patched$fips, md_patched$date),]
md <- md[order(md$state, md$fips, md$date),]

# Should be 1:1, yes
plot(md_patched$dowavg19_ACCIDENT[!md_patched$state %in% c('CT','NY', 'MA')], 
     md$dowavg19_ACCIDENT[!md$state %in% c('CT','NY', 'MA')])

# Fixed for may/june
plot(md_patched$dowavg19_ACCIDENT[md_patched$state %in% c('CT','NY', 'MA')], 
     md$dowavg19_ACCIDENT[md$state %in% c('CT','NY', 'MA')])

stopifnot(dim(md_patched) == dim(md))

write.csv(md_patched, file = file.path(output.loc, latest_refresh_day, 'patch', 'Waze_2020_MSA_day.csv'), row.names = F)    

# For MSA_week ----

# Make week sums
d18cw <- d_cbsa %>%
  filter(year == 2018 & month %in% c('05', '06') & state %in% c('CT', 'NY', 'MA')) %>%
  group_by(fips, alert_type, state, week,
  ) %>%
  summarize(weeksum18 = sum(count, na.rm = T))

d18cww <- d18cw %>%
  mutate(weeksum18 = ifelse(weeksum18 < 10, NA, weeksum18)) %>%
  pivot_wider(names_from = alert_type,
              values_from = weeksum18,
              names_prefix = 'weeksum18_')

mw_to_patch <- mw %>% filter(!state %in% c('CT','NY', 'MA'))
mw_patch <- mw %>% filter(state %in% c('CT','NY', 'MA'))

mw_patch <- mw_patch %>%
  left_join(d18cww)

mw_patch <- mw_patch %>%
  mutate(weeksum19_ACCIDENT = ifelse(is.na(weeksum18_ACCIDENT), weeksum19_ACCIDENT, weeksum18_ACCIDENT),
         weeksum19_JAM = ifelse(is.na(weeksum18_JAM), weeksum19_JAM, weeksum18_JAM),
         weeksum19_WEATHERHAZARD = ifelse(is.na(weeksum18_WEATHERHAZARD), weeksum19_WEATHERHAZARD, weeksum18_WEATHERHAZARD)) %>%
  dplyr::select(-weeksum18_ACCIDENT,
                -weeksum18_JAM,
                -weeksum18_WEATHERHAZARD)

# append to the to_patch df

mw_patched <- rbind(mw_to_patch, mw_patch)

mw_patched <- mw_patched[order(mw_patched$state, mw_patched$fips, mw_patched$week),]
mw <- mw[order(mw$state, mw$fips, mw$week),]

# Should be 1:1, yes
plot(mw_patched$weeksum19_ACCIDENT[!mw_patched$state %in% c('CT','NY', 'MA')], 
     mw$weeksum19_ACCIDENT[!mw$state %in% c('CT','NY', 'MA')])

# Fixed for may/june
plot(mw_patched$weeksum19_ACCIDENT[mw_patched$state %in% c('CT','NY', 'MA')], 
     mw$weeksum19_ACCIDENT[mw$state %in% c('CT','NY', 'MA')])

stopifnot(dim(mw_patched) == dim(mw))

write.csv(mw_patched, file = file.path(output.loc, latest_refresh_day, 'patch', 'Waze_2020_MSA_week.csv'), row.names = F)    


