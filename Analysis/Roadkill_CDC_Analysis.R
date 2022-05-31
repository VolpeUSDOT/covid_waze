# Summarize Roadkill alerts for CDC team
library(readxl)
library(tidyverse)

counties <- read_excel('Data/ORV_countystate_21.xls') %>%
  filter(!duplicated(GEOID))

# View(counties)

load('Data/Compiled_county_roadkill_counts_2021-11-08.RData')

# Create GEOID and filter to rabies surveillance counties

d <- compiled_counts %>%
  mutate(GEOID = paste0(STATEFP, COUNTYFP)) %>%
  filter(GEOID %in% all_of(counties$GEOID))

stopifnot(identical(sort(unique(d$GEOID)), sort(unique(counties$GEOID))))

# Tabulate by year/month

d <- d %>%
  left_join(counties) %>%
  mutate(ym = substr(yearday, 1, 7))

ym_summary <- d %>%
  group_by(NAME_1, STATEFP, COUNTYFP, GEOID, ym) %>%
  summarize(sum_count = sum(count),
            samp = n()) %>%
  arrange(desc(sum_count)) %>%
  slice_head(n = 5)

write.csv(ym_summary, 'Data/Top_5_Months_Roadkill_Sum_Counts_County.csv', row.names = F)

# Tabulate by hour of day

hr_summary <- d %>%
  group_by(NAME_1, STATEFP, COUNTYFP, GEOID, hour) %>%
  summarize(sum_count = sum(count),
            samp = n()) %>%
  arrange(desc(sum_count)) %>%
  slice_head(n = 5)

write.csv(hr_summary, 'Data/Top_5_Hours_Roadkill_Sum_Counts_County.csv', row.names = F)

# Viz over date by county
d <- d %>%
  mutate(date = as.Date(yearday))

ymd_summary <- d %>%
  group_by(NAME_1, STATEFP, COUNTYFP, GEOID, date) %>%
  summarize(sum_count = sum(count),
            samp = n())

# Expand the grid to include zero days
total_days = sort(unique(d$yearday))

tots <- gl(n = length(total_days),
           k = length(unique(d$GEOID)),
           labels = total_days)

total_counties = rep(unique(d$GEOID), length(total_days))

total_df <- data.frame(GEOID = total_counties,
                       date = as.Date(tots))

d_z <- total_df %>% 
  left_join(ymd_summary %>% select(NAME_1, STATEFP, COUNTYFP, GEOID) %>% filter(!duplicated(GEOID)))

d_z <- d_z %>%
  left_join(ymd_summary)

d_ze <- d_z %>%
  replace_na(list(sum_count = 0, samp = 0))

# Loop over counties and make a separate ts plot per county

pdf(file = 'Output/Waze_Roadkill_County_Date.pdf',
    width = 8, height = 5)

for(i in sort(unique(d$GEOID))){
  # i = unique(d$GEOID)[30]
  
  df_subset = d_ze %>% filter(GEOID == i) %>%
    filter(date > '2020-10-31')
  
  gp <- ggplot(df_subset, aes(x = date, y = sum_count)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_x_date(date_breaks = '3 month' 
                 , minor_breaks = '1 month'
                 , date_labels = format("%Y-%m")
                 ) +
    ggtitle(paste('Sum of Waze roadkill alerts by date for', unique(df_subset$NAME_1), i, 
                  '\n', sum(df_subset$sum_count > 0), 'days in last year with at least 1 alert'))
    
  
  print(gp)
}

dev.off()


# Average pct of days with at least 1 obs?

d_ze %>%
  group_by(GEOID) %>%
  summarize(n_days = n(),
            obs_days = sum(sum_count > 0),
            pct_obs = obs_days / n_days) %>%
  ungroup() %>%
  summarize(min_pct = min(pct_obs),
            med_pct = median(pct_obs),
            mean_pct = mean(pct_obs),
            max_pct = max(pct_obs))
