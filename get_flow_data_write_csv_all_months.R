# Get flow data for Nicola Chinook analysis
rm(list=ls())
library(tidyhydat)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(EflowStats)

# Get hydrology and weather data for covariates ---------------
# Hydrology - flow in Nicola River at spence's bridge, get mean and max flow for each month/year
# mean flow for each month
d_mean <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date, abbr=TRUE, label=TRUE)) %>%  
  filter(Parameter=="Flow") %>%
  group_by(year, month) %>% 
  summarise( mean_flow=mean(Value, na.rm=TRUE)) %>%
  spread(., month, mean_flow) 
names(d_mean)[2:13] <- tolower(paste0(names(d_mean)[2:13], "_mean_flow"))

#Sep-Oct 10th quantile flow for spawning
d_Q10 <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date, abbr=TRUE, label=TRUE)) %>%  
  filter(Parameter=="Flow", month %in% c("Sep", "Oct")) %>%
  group_by(year) %>% 
  summarise( Q10_flow=quantile(Value, probs=0.1, na.rm=TRUE)) 
names(d_Q10)[2] <- paste0("sep_oct_Q10_flow")

# Sep-Dec max flow for spawning
d_max <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date, abbr=TRUE, label=TRUE)) %>%  
  filter(Parameter=="Flow", month %in% c("Sep", "Oct", "Nov", "Dec")) %>%
  group_by(year) %>% 
  summarise( max_flow=max(Value, na.rm=TRUE)) 
names(d_max)[2] <- paste0("sep_dec_max_flow")


# d_max <- hy_daily_flows(station_number= "08LG006") %>% 
#   mutate(year = year(Date), month= month(Date, abbr=TRUE, label=TRUE)) %>%
#   mutate(season = )
#   filter(Parameter=="Flow") %>%
#   group_by(year, month) %>% 
#   summarise( max_flow=max(Value, na.rm=TRUE)) %>% 
#   spread(., month, max_flow) 
# names(d_max)[2:13] <- tolower(paste0(names(d_max)[2:13], "_max_flow"))

d_spawn1 <- merge(d_mean, d_Q10, by="year")
d_spawn <- merge(d_spawn1, d_max, by="year")

# check to make sure fall variables aren't correlated
#plot(d_spawn$sep_oct_Q10_flow, d_spawn$sep_dec_max_flow )

d_rear <- d_spawn
d_rear$year = d_rear$year - 1 # make year for rearing parameters for brood year one year ago
names(d_rear)[2:15] <- paste0(names(d_rear)[2:15], "_rear")

d <- merge(d_spawn, d_rear, by="year")

write.csv(d, "./data/nicola_yearly_flows_all_months.csv", row.names=FALSE)

