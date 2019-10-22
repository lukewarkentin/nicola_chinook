# Get flow data for Nicola Chinook analysis
rm(list=ls())
library(tidyhydat)
library(dplyr)
library(lubridate)
library(ggplot2)

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
  summarise(mean_flow_aug_rear = mean(Value))
# shift aug flow data so that it is for the summer of rearing for each brood year
# so have the aug flow for the summer of 2015 be on brood year 2014 (so 2015 is now 2014)
mean_aug_flow_nicola$year <- mean_aug_flow_nicola$year - 1
d <- left_join(max_flow_winter_nicola, mean_aug_flow_nicola, by="year")

# Get average Sep-Oct flow for spawning year
mean_sep_oct_flow_nicola <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date)) %>% 
  filter(month %in% c(9,10), Parameter=="Flow") %>%
  group_by(year) %>%
  summarise(mean_sep_oct_flow = mean(Value))
d <- left_join(d, mean_sep_oct_flow_nicola, by="year")

# Get average aug flow for spawning year
mean_aug_flow_nicola <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date)) %>% 
  filter(month ==8, Parameter=="Flow") %>%
  group_by(year) %>%
  summarise(mean_flow_aug_spawn = mean(Value))
d <- left_join(d, mean_aug_flow_nicola, by="year")

# Get max Jan-Feb flow for rearing year
max_jan_feb_flow_nicola <- hy_daily_flows(station_number= "08LG006") %>% 
  mutate(year = year(Date), month= month(Date)) %>% 
  filter(month %in% c(1,2), Parameter=="Flow") %>%
  group_by(year) %>%
  summarise(max_jan_feb_flow = max(Value))
# shift jan_feb flow data so that it is for the winter of incubating for each brood year
# so have the jan_feb flow for the winter of 2015 be on brood year 2014 (so 2015 is now 2014)
max_jan_feb_flow_nicola$year <- max_jan_feb_flow_nicola$year - 1
d <- left_join(d, max_jan_feb_flow_nicola, by="year")



write.csv(d, "./data/nicola_yearly_flows.csv", row.names=FALSE)


#get full time series of flow data for Nicola at Spences Bridge and Spius and COldwater
fd <- hy_daily_flows(station_number= c("08LG006", 
                     # "08LG008", # SPIUS CREEK NEAR CANFORD # Missing 2009-2010
                     # "08LG010", # COLDWATER RIVER AT MERRITT) # Missing 1996-2004
                     "08LG048") # COLDWATER RIVER NEAR BROOKMERE 
)
fd <- merge(fd, allstations[names(allstations) %in% c("STATION_NAME", "STATION_NUMBER")], by="STATION_NUMBER")
# Add year, year-day, and month columns
fd$year <- year(fd$Date)
fd$yday <- yday(fd$Date)
fd$month <- month(fd$Date, abbr=TRUE, label=TRUE)
# get observations by month
month_tab <- table(fd$year, fd$month)
# Get vector of years with complete August records
complete_aug <- which(month_tab[ ,8] >30)
complete_aug <- names(complete_aug)
# Get vector of years with complete Oct 1- Dec 31 records (fall)
complete_fall <- apply(month_tab[,10:12], 1, function(x) all(x>29))
complete_fall <- names(complete_fall[complete_fall==TRUE])
# just get years with complete aug and fall periods
d1 <- d[d$year %in% complete_aug & d$year %in% complete_fall, ]
# get only rows with no NAs
d2 <-  d1[which(apply(d1, 1, function(i) all(!is.na(i)))),]
# Plot correlation between max fall flow and aug flow in rearing, for each brood year
png("./figures/fig_correlation_fall_aug_flows.png",  res=100)
plot(d2$max_flow_fall, d2$mean_flow_aug_rearing, ylab="Mean fall flow, rearing summer (m3/s)", xlab="Max fall flow, spawning (m3/s)")
points(d2$max_flow_fall[d2$year>=1995 & d2$year<=2013], d2$mean_flow_aug_rearing[d2$year>=1995 & d2$year<=2013], col="red")
dev.off()
cor(d2$max_flow_fall, d2$mean_flow_aug_rearing)

#plot time series
library(ggplot2)
# Plot August flows over time
rg1 <- range(fd_complete_aug$year)
fig_aug_flows <-  ggplot(fd_complete_aug[ fd_complete_aug$month %in% 8, ], aes(x=year, y=Value)) + 
  geom_point(shape=46) +
  geom_boxplot(aes(group=year)) +
  geom_smooth(method="lm") +
  scale_x_discrete(breaks=seq(rg1[1], rg1[2], 1), limits=seq(rg1[1], rg1[2], 1)) +
  stat_summary(geom="point", fun.y="mean", colour="red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
fig_aug_flows

# Plot fall flows over time
rg <- range(fd_complete_fall$year) # get year range for fall time series
fig_fall_flows <- ggplot(fd_complete_fall[ fd_complete_fall$month %in% c(10:12), ], aes(x=year, y=Value)) + 
  geom_point(shape=46) +
  geom_boxplot(aes(group=year)) +
  geom_smooth(method="lm") +
  scale_x_discrete(breaks=seq(rg[1], rg[2], 1), limits=seq(rg[1], rg[2], 1)) +
  stat_summary(geom="point", fun.y="max", colour="red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
fig_fall_flows

# Look at winter flows for years 1990 to 2015
fig_late_winter_flows <- fd %>% filter(month %in% c("Jan", "Feb", "Mar") & year %in% c(1995:2014)) %>%
  ggplot(., aes(y=Value, x=Date, colour=Symbol)) +
  geom_point() +
  facet_grid(STATION_NAME~year, scales="free_x") +
  scale_x_date(date_breaks="1 month", date_labels = "%e %b %y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
fig_late_winter_flows 

# Look at early winter flows for years 1990 to 2015
fig_early_winter_flows <- fd %>% filter(month %in% c("Nov", "Dec") & year %in% c(1995:2014)) %>%
  ggplot(., aes(y=Value, x=Date, colour=Symbol)) +
  geom_point() +
  facet_grid(STATION_NAME~year, scales="free_x") +
  scale_x_date(date_breaks="1 month", date_labels = "%e %b %y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
fig_early_winter_flows

# Look fall flows for years 1990 to 2015
fig_fall_flows <- fd %>% filter(month %in% c("Sep", "Oct") & year %in% c(1995:2014)) %>%
  ggplot(., aes(y=Value, x=Date, colour=Symbol)) +
  geom_point() +
  facet_grid(STATION_NAME~year, scales="free_x") +
  scale_x_date(date_breaks="1 month", date_labels = "%e %b %y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
fig_fall_flows

# Look summer flows for years 1990 to 2015
fig_summer_flows <- fd %>% filter(month %in% c("Jul", "Aug") & year %in% c(1995:2014)) %>%
  ggplot(., aes(y=Value, x=Date, colour=Symbol)) +
  geom_point() +
  facet_grid(STATION_NAME~year, scales="free_x") +
  scale_x_date(date_breaks="1 month", date_labels = "%e %b %y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
fig_summer_flows

#save figures
rm(fig_list)
fig_list <- mget(ls(pattern="fig_")) #make list of all figures
invisible(mapply(ggsave, file=paste0("./figures/", names(fig_list), ".png"), height= 4, width=8, plot=fig_list)) #save list of figures
