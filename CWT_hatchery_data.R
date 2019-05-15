# Investigate CWT harchery data from RMIS

rm(list=ls())
setwd("D:/22_masters/100.002_Nicola-Chinook-cohorts/10_R/nicola_chinook")

library(lubridate)
library(ggplot2)
library(tidyr)

# Read in data, dowloaded from RMIS: https://www.rmis.org/cgi-bin/queryfrm.mpl?Table=all_releases&Version=4.1

# NOTE: this data includes releases to Spius Creek, Coldwater River, and Maka Creek, which the data Doug Lofthouse sent me 2019-02-07 does not include. 
d <- read.csv("./data/RMIS-chinook-CWT-data.txt", stringsAsFactors = FALSE)

# Process data - only want Nicola, Coldwater, and Spius stocks
unique(d$stock_location_name)
# remove Bonaparte and Deadman
d1 <- d[ which(d$stock_location_name %in% c("S-Bonaparte R", "S-Deadman R") == FALSE), ]
unique(d1$release_location_name) # check location names
unique(d1$hatchery_location_name) # Note there are some releases from Merritt Schools and Nicola River Inc.
# only include spius creek releases
d1 <- d1[d1$hatchery_location_name =="H-Spius Creek H",]


# Fix dates that don't have days
# For releases without day (only year and month), make them day 01
d1$first_release_date <- sapply(d1$first_release_date, function(i) {
  new <- ifelse(nchar(i)== 8, i, paste0(i, "01"))
  return(new)
}
)
d1$last_release_date <- sapply(d1$last_release_date, function(i) {
  new <- ifelse(nchar(i)== 8, i, paste0(i, "01"))
  return(new)
}
)
# Make dates into date format
d1$first_release_date <- ymd(d1$first_release_date)
d1$last_release_date <- ymd(d1$last_release_date)

str(d1)
unique(d1$release_stage)
unique(d1$run)
unique(d1$release_location_name)
write.csv(d1, "check-data.csv")

#Get one count column - wide to long format
to_gather <- tail(names(d1), n=5) #get last 5 column names - these hold the count data
d1 <- gather(d1, key="tag_mark_type", value="fish_count", to_gather)

# Add day of year for first and last release date
d1$first_release_day <- yday(d1$first_release_date)
d1$last_release_day <- yday(d1$last_release_date)

# add release year column
d1$release_year  <- year(d1$first_release_date)
unique(d1$release_location_name)

# Visualize ------
# Plot releases by type for each year
fig_hatchery_stage_only <- ggplot(d1, aes(y=fish_count, x=release_year, fill=release_stage)) +
  geom_col() +
  scale_y_continuous(labels=scales::comma, breaks=seq(200000, 800000, by=200000)) + 
  theme_bw()
fig_hatchery_stage_only

fig_hatchery_stage_only_stocks <- ggplot(d1, aes(y=fish_count, x=release_year, fill=release_stage)) +
  geom_col() +
  scale_y_continuous(labels=scales::comma, breaks=seq(200000, 800000, by=200000)) + 
  facet_wrap(~interaction(stock_location_name, release_location_name)) +
  theme_bw()
fig_hatchery_stage_only_stocks

fig_hatchery_stage_mark <- ggplot(d1, aes(y=fish_count, x=release_year, fill=tag_mark_type)) +
  geom_col() +
  scale_y_continuous(labels=scales::comma) + 
  facet_grid(release_stage~.) +
  theme_bw()
fig_hatchery_stage_mark

fig_release_day_of_year <- ggplot(d1[d1$fish_count>0, ], aes(y=fish_count, x=first_release_day, colour=release_stage)) +
  geom_point() +
  geom_segment(aes(xend=last_release_day, yend=fish_count)) +
  scale_y_continuous(labels=scales::comma) +
  theme_bw()
fig_release_day_of_year

#save figures
rm(fig_list)
fig_list <- mget(ls(pattern="fig_.*")) #make list of all figures
invisible(mapply(ggsave, file=paste0(names(fig_list), ".png"), path="./figures", height= 6, width=10, plot=fig_list)) #save list of figures




