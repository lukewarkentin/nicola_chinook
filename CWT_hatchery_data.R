# Investigate CWT harchery data from RMIS

rm(list=ls())
setwd("D:/22_masters/100.002_Nicola-Chinook-cohorts/10_R/nicola_chinook")

library(lubridate)

# Read in data, dowloaded from RMIS: https://www.rmis.org/cgi-bin/queryfrm.mpl?Table=all_releases&Version=4.1
d <- read.csv("RMIS-chinook-CWT-data.txt", stringsAsFactors = FALSE)

# Process data - only want Nicola, Coldwater, and Spius stocks
unique(d$stock_location_name)
# remove Bonaparte and Deadman
d1 <- d[ which(d$stock_location_name %in% c("S-Bonaparte R", "S-Deadman R") == FALSE), ]
unique(d1$release_location_name) # check location names
unique(d1$hatchery_location_name) # Note there are some releases from Merritt Schools and Nicola River Inc.



