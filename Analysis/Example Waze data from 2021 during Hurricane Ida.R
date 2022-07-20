### Set up ----
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(maps)
library(dplyr)
library(magick)
library(RColorBrewer)

auto_export_bucket = 's3://prod-sdc-waze-autoexport-004118380849/alert/'

volpe_drive = '//vntscex.local/DFS/Projects/PROJ-OS62A1/SDI Waze Phase 2/Output/COVID'

jessie_dr = 'C:/Users/Jessie.Yang.CTR/Downloads/2022 Projects/BTS Waze'

nd <- read_csv(file.path(volpe_drive, 'Waze_2020_National_day.csv'))   
nw <- read_csv(file.path(volpe_drive, 'Waze_2020_National_week.csv'))
all <- read_csv(file.path(volpe_drive, 'Waze_Full.csv'))

# Visualize the county data
ct <- all %>% mutate(date= as.Date(date)) %>% ungroup() %>% group_by(date) %>% summarize(total_count = n(), sum_waze_counts = sum(count, na.rm = T))
gp <- ggplot(ct, aes(x = date, y = sum_waze_counts)) + geom_line() + xlim(Sys.Date() - 60, Sys.Date())
plotly::ggplotly(gp)


# by county, date
library(tidyverse)
library(plotly)

maps::county.fips %>%
  as.tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") ->
  dfips

map_data("county") %>% 
  left_join(dfips) -> dall

dall %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color= "gray70") +
  coord_map() +
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray90"))
  
# plot the data by county and date
la <- read.csv(file.path(jessie_dr, "LA_hurricane_export","LA_hurricane_processed.csv"))
lasub <- la %>% mutate (fips = paste0(sprintf("%02d", STATEFP), sprintf("%03d", COUNTYFP))) %>% 
  mutate(date= as.Date(yearday), fips = as.numeric(fips)) %>% 
  filter(date <= '2021-08-31' & date >= '2021-08-15') %>% 
  group_by(date, fips,sub_type) %>% 
  summarize(total_count = n(), sum_waze_counts = sum(count, na.rm = T))  

dt <- dall%>% left_join(lasub) %>% mutate()



gp <- dt %>% 
  filter(date == '2021-08-31')  %>% 
  ggplot(aes(long, lat, group = group, text = paste(date,":",sub_type))) +
  geom_polygon(aes(fill = sum_waze_counts), color = "gray70") +
  #geom_polygon(data = dall, color = "gray70") +
  coord_map() + scale_colour_manual(values = rev(brewer.pal(3, "BuPu"))) + 
  facet_wrap(~ sub_type) + ggtitle('2021-08-31')
  
gp
ggplotly(gp)


unique(la$sub_type) # 25 sub-types.

# Create a gif file
for (y in as.character(unique(dt$date[!is.na(dt$date)]))) {
  
  gp <- dt %>% 
    filter(date == y)  %>% 
    ggplot(aes(long, lat, group = group, text = paste(date,":",sub_type))) +
    geom_polygon(aes(fill = sum_waze_counts), color = "gray70") +
    coord_map() + scale_colour_manual(values = rev(brewer.pal(3, "BuPu"))) + 
    facet_wrap(~ sub_type) + ggtitle(paste0("All type ", y))
  
  fp <- file.path(jessie_dr, paste0("All type", y, ".png"))
  
  ggsave(plot = gp, 
         filename = fp, 
         device = "png", width=10, height=8, dpi=300)
  
}

# make a gif using the png files
list.files(path=jessie_dr, pattern = '^All type', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps = 0.5) %>% # animates, can opt for number of loops
  image_write(file.path(jessie_dr, "All type.gif")) # write to current dir


# make a gif with only one sub-type
for (y in as.character(unique(dt$date[!is.na(dt$date)]))) {
  subtype = "HAZARD_WEATHER_FLOOD"
  
  if (dim(dt %>% 
             filter(date == y & sub_type == subtype))[1] != 0){
    gp <- dt %>% 
      filter(date == y & sub_type == subtype)  %>% 
      ggplot(aes(long, lat, group = group, text = paste(date,":",sub_type))) +
      geom_polygon(aes(fill = sum_waze_counts), color = "gray70") +
      coord_map() + scale_colour_manual(values = rev(brewer.pal(3, "BuPu"))) + 
      #facet_wrap(~ sub_type) + 
      ggtitle(paste0(subtype," ", y))
    
    fp <- file.path(jessie_dr, paste0(subtype," ", y, ".png"))
    
    ggsave(plot = gp, 
           filename = fp, 
           device = "png", width=10, height=8, dpi=300)
  }
  
}

# make a gif using the png files
list.files(path=jessie_dr, pattern = '^HAZARD_WEATHER_FLOOD', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps = 0.5) %>% # animates, can opt for number of loops
  image_write(file.path(jessie_dr, paste0(subtype,".gif"))) # write to current dir




############ tests ###########################
# Plot in a map
data(county.fips)

## Set up fake df_pop_county data frame
df_pop_county <- data.frame(region=county.fips$fips)
df_pop_county$value <- county.fips$fips
y <- df_pop_county$value
df_pop_county$color <- gray(y / max(y))

## merge population data with county.fips to make sure color column is
## ordered correctly.
counties <- county.fips %>% left_join(df_pop_county, by=c('fips'='region'))
map("county", fill=TRUE, col=counties$color)




