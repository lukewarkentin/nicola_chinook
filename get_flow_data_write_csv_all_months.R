# Get flow data for Nicola Chinook analysis
library(tidyhydat)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(EflowStats)
rm(list=ls())

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

# Jan-Feb max flow after spawning
d_max_winter <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date)-1, month= month(Date, abbr=TRUE, label=TRUE)) %>%  # subtracting 1 year, so that winter flood in January 2011 is for brood year 2010
  filter(Parameter=="Flow", month %in% c("Jan", "Feb")) %>%
  group_by(year) %>% 
  summarise( max_flow=max(Value, na.rm=TRUE)) 
names(d_max_winter)[2] <- paste0("jan_feb_max_flow")


#get Ice days
ice <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = get_waterYear(Date)) %>%   # use get_waterYear to get the year of the January of the winter after brood year - this will include Oct-Dec 2018 and Jan 2019 all as water-year 2019
  group_by(year) %>% 
  filter(Parameter=="Flow")  
ice_df <- as.data.frame.matrix(t(table(ice$Symbol, ice$year)))
ice_df$year <- as.numeric(row.names(ice_df))
ice_df <- ice_df[,c("year", "B")]
names(ice_df)[2] <- "ice_days"
ice_df$year <- ice_df$year - 1 # make year of ice-free days for the brood year (previous year)

# d_max <- hy_daily_flows(station_number= "08LG006") %>% 
#   mutate(year = year(Date), month= month(Date, abbr=TRUE, label=TRUE)) %>%
#   mutate(season = )
#   filter(Parameter=="Flow") %>%
#   group_by(year, month) %>% 
#   summarise( max_flow=max(Value, na.rm=TRUE)) %>% 
#   spread(., month, max_flow) 
# names(d_max)[2:13] <- tolower(paste0(names(d_max)[2:13], "_max_flow"))

d_spawn1 <- merge(d_mean, d_Q10, by="year", all=TRUE)
d_spawn2 <- merge(d_spawn1, d_max_winter, by="year", all=TRUE)
d_spawn3 <- merge(d_spawn2, ice_df, by="year", all=TRUE)
d_spawn <- merge(d_spawn3, d_max, by="year", all=TRUE)

# check to make sure fall variables aren't correlated
#plot(d_spawn$sep_oct_Q10_flow, d_spawn$sep_dec_max_flow )

d_rear <- d_spawn
d_rear$year = d_rear$year - 1 # make year for rearing parameters for brood year one year ago
names(d_rear)[2:ncol(d_rear)] <- paste0(names(d_rear)[2:ncol(d_rear)], "_rear")

d <- merge(d_spawn, d_rear, by="year", all=TRUE)


# Clip to only full years
# Get all the flow data
fd <- hy_daily_flows(station_number= "08LG006")
fd$year <- year(fd$Date)
fd$month <- month(fd$Date)

# get observations by month
month_tab <- table(fd$year, fd$month)
# Get vector of years with complete years
d_full_yr <- d[d$year %in% as.numeric(names(which(table(fd$year)>=365))), ]


write.csv(d_full_yr, "./data/nicola_yearly_flows_all_months.csv", row.names=FALSE)


