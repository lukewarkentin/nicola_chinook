#examine flow data

rm(list=ls())

library(tidyhydat)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(viridis)
library(ggplot2)
library(weathercan)
library(ggmap)
library(stringr)
library(EflowStats)

### Flow data --------
#make vector of all sites in Nicola watershed
hyd_sites <- c("08LG048", 
               "08LG010", 
               #"08LG028", #this is NICOLA RIVER AT OUTLET OF DOUGLAS LAKE
               #"08LG069", #missing?
               #"08LG046",  #NICOLA RIVER NEAR QUILCHENA no data in hydat
               "08LG065",
               "08LG008",
               "08LG006",
               "08LG041")

hy_stn_data_range(hyd_sites)
fd <- hy_daily_flows(hyd_sites)
fd <- left_join(fd, allstations, by="STATION_NUMBER")

str(fd) 
unique(fd$Parameter)
fd$yday <- yday(fd$Date)
fd$year <- year(fd$Date)
fd$water_year <- get_waterYear(fd$Date)
fd$month <- month(fd$Date, label=TRUE, abbr=TRUE)
fd$decade <- paste0(str_sub(fd$year, 1, 3), "0")
str(fd)

# Look at span of flow data -----------
avail_flow_data <- table(fd$year, fd$STATION_NAME)
write.csv(avail_flow_data, "./avail_flow_data_all_stations.csv")
yrs <- as.data.frame(avail_flow_data)

# get years with complete data
names(yrs) <- c("year", "STATION_NAME", "days_record")
yrs$complete <- ifelse(yrs$days_record >360, "Y", "N")


fd <- merge(fd, yrs, by=c("STATION_NAME", "year"), all.x=TRUE)

# plot availability of flow data from hydat
ggplot(fd, aes(y=STATION_NAME, x=Date)) + 
  geom_point() +
  #scale_x_log10(breaks=seq(1910, 2020, by=5)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle=90, hjust=0.5))

# summarise yearly max flowsfor each site
#fds <- fd %>% group_by(STATION_NAME, year) %>% summarise(max_flow = max(Value, na.rm = TRUE), min_flow = min(Value, na.rm=TRUE), date_max =  )
fds <- fd %>% group_by(STATION_NAME, water_year) %>% slice(which.max(Value)) 
fdsmin <- fd %>% filter(month %in% c("Jul", "Aug")) %>% group_by(STATION_NAME, water_year) %>% slice(which.min(Value))
fdsmin_winter <-fd  %>% filter(month %in% c("Nov", "Dec", "Jan", "Feb", "Mar")) %>% group_by(STATION_NAME, water_year) %>% slice(which.min(Value))
  

#plot max flows for each site
fig_max_flows_all_sites <- ggplot(fds, aes(y=Value, x=water_year)) +
  geom_point(aes(colour=month), size=2) +
  geom_line() +
  geom_text(aes(label=month), size=2) +
  facet_wrap(~STATION_NAME, scales="free_y") + 
  theme_classic()
ggsave("./figures/fig_max_flows_all_sites.png", fig_max_flows_all_sites )

#plot max flows for each site, only for complete record years
fig_max_flows_all_sites_full_years_only <- ggplot(fds[fds$complete=="Y", ], aes(y=Value, x=water_year)) +
  geom_point(aes(colour=month), size=2) +
  geom_line() +
  geom_text(aes(label=month), size=2) +
  facet_wrap(~STATION_NAME, scales="free_y") + 
  theme_classic() 
ggsave("./figures/fig_max_flows_all_sites_full_years_only.png", fig_max_flows_all_sites_full_years_only)

#plot min summer flows for each site
fig_min_summer_flow_all_sites <- ggplot(fdsmin, aes(y=Value, x=water_year)) +
  geom_point(aes(colour=month), size=2) +
  geom_line() +
  geom_text(aes(label=month), size=2) +
  facet_wrap(~STATION_NAME, scales="free_y") + 
  theme_classic() 
ggsave("./figures/fig_min_summer_flow_all_sites.png", fig_min_summer_flow_all_sites)

# plot min winter flows for each site
fig_min_winter_flows_all_sites <- ggplot(fdsmin_winter, aes(y=Value, x=water_year)) +
  geom_point(aes(colour=month), size=2) +
  geom_line() +
  geom_text(aes(label=month), size=2) +
  facet_wrap(~STATION_NAME, scales="free_y") + 
  theme_classic() 
ggsave("./figures/fig_min_winter_flows_all_sitess.png", fig_min_winter_flows_all_sites)


