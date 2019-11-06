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
fd$decade <- round(fd$year, digits = -1)
fd$water_year <- get_waterYear(fd$Date)
fd$year_day <- yday(fd$Date)
  
# get observations by month
month_tab <- table(fd$year, fd$month)
month_tab

table(fd$year, fd$month)

# Get vector of years with complete years
d_full_yr <- d[d$year %in% as.numeric(names(which(table(fd$year)>=365))), ]

write.csv(d_full_yr, "./data/nicola_yearly_flows_all_months.csv", row.names=FALSE)

# Plot full time series of hydrometric data, selecting for full falls/ Augusts/ winters as appropriate
# Try polar plot of flow data
str(fd)

days_month <- as.vector(table(fd$month, fd$year)[,"1970"])
days_month 
month_breaks <- cumsum(days_month)-31 + 1
month_labels <- format(ISOdatetime(2000,1:12,1,0,0,0),"%b")

myPalette <- colorRamps::matlab.like2(10) # get colour palette
myPalette

fig_hydro_circle <- fd %>% # [fd$year %in% as.numeric(names(which(table(fd$water_year)>=365))), ] %>%
  ggplot(aes(y=Value, x=year_day, colour=factor(decade), group=year)) + 
  geom_path( size=1.2, alpha=0.5) +
  #stat_summary(fun.y="mean", geom="path", size=2, alpha=0.6) +
  scale_colour_manual(values=myPalette) +
  coord_polar() + 
  scale_y_continuous(limits=c(-200,max(fd$Value)), expand=c(0,0)) +
  #scale_y_continuous(limits=c(-300,100), expand=c(0,0)) +
  scale_x_continuous(breaks=month_breaks, labels=month_labels, minor_breaks=NULL) +
  geom_hline(aes(yintercept=0)) +
  theme_bw()
fig_hydro_circle
ggsave("./figures/fig_hydro_circle.png", fig_hydro_circle)

# August flows
fig_aug_flow_dist <- fd[fd$year %in% as.numeric(names(which(table(fd$year, fd$month)[,8]==31))) &  # get only years with complete augusts
     fd$month==8 , # Get only august observations
   ] %>% 
  ggplot(., aes(x=Value, colour=factor(decade))) +
  geom_density() +
  scale_colour_manual(values=myPalette) +
  #geom_histogram(bins=100) + 
  #facet_grid(decade~.) +
  theme_bw()
fig_aug_flow_dist
ggsave("./figures/fig_aug_flow_dist.png", fig_aug_flow_dist)

# Winter flows
fd[fd$year %in% as.numeric(names(which(table(fd$water_year)>=365))) &  # get only years with complete decembers
     fd$month %in% c(1:3, 12) , # Get only august observations
   ] %>% 
  ggplot(., aes(x=Value, colour=factor(decade))) +
  geom_density() +
  scale_x_continuous(limits=c(0,30)) +
  scale_colour_manual(values=myPalette) +
  #geom_histogram(bins=100) + 
  #facet_grid(decade~.) +
  theme_bw()

# March huge flood - which year?
check <- fd[fd$Value>100 & fd$month==3, ]

str(fd)
# monthly averages
fd %>% # [fd$year %in% as.numeric(names(which(table(fd$water_year)>=365))), ] %>%
  ggplot(aes(y=Value, x=month, colour=factor(decade), group=year)) + 
  #geom_path( size=1.2, alpha=0.5) +
  stat_summary(fun.y="mean", geom="path", size=2, alpha=0.3) +
  scale_colour_manual(values=myPalette) +
  #coord_polar() + 
  #scale_y_continuous(limits=c(-200,max(fd$Value)), expand=c(0,0)) +
  #scale_y_continuous(limits=c(-300,100), expand=c(0,0)) +
  scale_x_continuous(breaks=1:12, labels=month_labels, minor_breaks=NULL) +
  geom_hline(aes(yintercept=0)) +
  theme_bw()



# August flows boxplots
fd[fd$year %in% as.numeric(names(which(table(fd$year, fd$month)[,8]==31))) &  # get only years with complete augusts
     fd$month==8 , # Get only august observations
   ] %>% 
  ggplot(., aes(y=Value, x=year, group=year)) +
  geom_boxplot() +
  theme_bw()

# Ice days
ice_df$decade <- round(ice_df$year, digits=-1)
ice_df[ice_df$year %in% as.numeric(names(which(table(fd$water_year)>=365))),
    ] %>%  # get only years with complete years (winters)
  ggplot(., aes(x=ice_days)) +
  geom_histogram() +
  #geom_density() + 
  facet_grid(decade~.) +
  theme_bw()

# ice days time series
ice_df[ice_df$year %in% as.numeric(names(which(table(fd$water_year)>=365))),
       ] %>%  # get only years with complete years (winters)
  ggplot(., aes(x=year, y=ice_days)) +
  geom_point() +
  geom_line() +
  theme_bw()

# Winter flows
fd[fd$year %in% as.numeric(names(which(table(fd$water_year)>=365))) &  # get only years with complete decembers
     fd$month %in% c(1:3, 12) , # Get only august observations
   ] %>% 
  ggplot(., aes(x=Value)) +
  geom_histogram(bins=100) + 
  #scale_y_log10() +
  #scale_x_log10() +
  facet_grid(decade~.) +
  theme_bw()

# winter flows time series
fd[fd$year %in% as.numeric(names(which(table(fd$water_year)>=365))) &  # get only years with complete decembers
     fd$month %in% c(1:3, 11:12) , # Get only august observations
   ] %>% 
  ggplot(., aes(y=Value, x=year, group=year)) +
  geom_boxplot() + 
  theme_bw()

# winter flows median
fd[fd$year %in% as.numeric(names(which(table(fd$water_year)>=365))) &  # get only years with complete decembers
     fd$month %in% c(1:3, 11:12) , # Get only august observations
   ] %>% 
  ggplot(., aes(y=Value, x=year, group=year)) +
  stat_summary(fun.y = median, geom="point") + 
  theme_bw()

# get summary of median winter flow Nov-Mar
med_win_flow <- fd[fd$water_year %in% as.numeric(names(which(table(fd$water_year)>=365))) &  # get only years with complete decembers
     fd$month %in% c(1:3, 11:12) , # Get only winter observations
   ] %>% group_by(water_year) %>% summarise(median_flow = median(Value, na.rm=TRUE)) 

med_win_flow$year = med_win_flow$water_year -1 # subtract one from year to get brood year
win_sum <- merge(ice_df, med_win_flow, by="year", all.x=TRUE) # merge with ice days
plot(win_sum$ice_days, win_sum$median_flow)                 

# Max fall flood
d_max$decade <- round(d_max$year, digits=-1)
d_max[d_max$year %in% as.numeric(names(which(table(fd$water_year)>=365))),
       ] %>%  # get only years with complete years (winters)
  ggplot(., aes(x=sep_dec_max_flow)) +
  geom_histogram() +
  #geom_density() + 
  facet_grid(decade~.) +
  theme_bw()
# Fall daily
fd[fd$year %in% as.numeric(names(which(table(fd$year, fd$month)[,12]==31))) &  # get only years with complete decembers
     fd$month %in% 9:12 , # Get only august observations
   ] %>% 
  ggplot(., aes(x=Value)) +
  geom_histogram(bins=100) +
  scale_y_log10() +
  facet_grid(decade~.) +
  theme_bw()
