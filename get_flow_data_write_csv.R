# Get flow data for Nicola Chinook analysis

setwd("D:/22_masters/100.002_Nicola-Chinook-cohorts/10_R/nicola_chinook")
library(tidyhydat)
library(dplyr)
library(lubridate)

# Get hydrology and weather data for covariates ---------------
# Hydrology - flow in Nicola River at spence's bridge, maximum value October 1 - December 31, for each year  
max_flow_winter_nicola <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date)) %>%  
  mutate(winter = ifelse(month %in% c(10,11,12), 1, 0)) %>% # Oct-Dec flow only (no January flows)
  filter(winter==1, Parameter=="Flow") %>%
  group_by(year) %>% # Oct-Dec flows only (No January flows)
  summarise(max_flow_fall = max(Value))

# Get average august flow for drought index - try with July flow too
mean_aug_flow_nicola <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date)) %>% 
  filter(month==8, Parameter=="Flow") %>%
  group_by(year) %>%
  summarise(mean_flow_aug_rearing = mean(Value))
# shift aug flow data so that it is for the summer of rearing for each brood year
# so have the aug flow for the summer of 2015 be on brood year 2014 (so 2015 is now 2014)
mean_aug_flow_nicola$year <- mean_aug_flow_nicola$year - 1
d <- left_join(max_flow_winter_nicola, mean_aug_flow_nicola, by="year")

write.csv(d, "./data/nicola_yearly_flows.csv", row.names=FALSE)
