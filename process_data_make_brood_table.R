# Read in and process Nicola CWT Escapement Data - from Chuck Parken

rm(list=ls())
setwd("D:/22_masters/100.002_Nicola-Chinook-cohorts/10_R/nicola_chinook")

library(readxl)
library(lubridate)
library(tidyr)

# Hatchery release data ------
# Read in data, dowloaded from RMIS: https://www.rmis.org/cgi-bin/queryfrm.mpl?Table=all_releases&Version=4.1
# NOTE: this data includes releases to Spius Creek, Coldwater River, and Maka Creek
hd <- read.csv("./data/RMIS-chinook-CWT-hatchery-release-data.txt", stringsAsFactors = FALSE)
# Process data - only want Nicola, Coldwater, and Spius stocks
# remove Bonaparte and Deadman
# d <- d[ which(d$stock_location_name %in% c("S-Bonaparte R", "S-Deadman R") == FALSE), ]
hd[!hd$hatchery_location_name=="H-Spius Creek H",] # check releases from other sources
# Merritt School released 80 untagged/unclipped 0+ smolts in 1993 and 80 untagged/unclipped fry in 1995. 
# Nicola Inc. released 18000 untagged/unclipped eggs in 1981 and 2500 untagged/unclipped eggs in 1982.
# I think it's safe to remove these rows.
# only include spius creek 
hd <- hd[hd$hatchery_location_name =="H-Spius Creek H",]
# Include only Nicola Stock for analysis
hd <- hd[hd$stock_location_name=="S-Nicola R", ]

# Fix dates that don't have days
# For releases without day (only year and month), make them day 01
hd$first_release_date <- sapply(hd$first_release_date, function(i) {
  new <- ifelse(nchar(i)== 8, i, paste0(i, "01"))
  return(new)
}
)
hd$last_release_date <- sapply(hd$last_release_date, function(i) {
  new <- ifelse(nchar(i)== 8, i, paste0(i, "01"))
  return(new)
}
)
# Make dates into date format
hd$first_release_date <- ymd(hd$first_release_date)
hd$last_release_date <- ymd(hd$last_release_date)
str(hd)

#Get one count column - wide to long format
to_gather <- tail(names(hd), n=5) #get last 5 column names - these hold the count data
hdl <- gather(hd, key="tag_mark_type", value="fish_count", to_gather)
str(hdl)

# Adult CWT Returns - Mark-recapture program ----------
adm <- read_excel("./data/Nicola R  Escapement CWT Data 1987-2018.xlsx", sheet = "Nicola - Escapement CWT Data")
str(adm)



# Adult CWT Returns - Take by hatchery for brood stock ----------
adh <- read_excel("./data/Hatchery Removals by Nicola CWT code.xlsx", skip=4)
str(adh)
adh <- adh[-grep("Total", adh$BROOD_YEAR), -grep("Grand Total", names(adh))] # remove rows with yearly totals and column of grand total
head(adh)

# fill in missing years
adh$BROOD_YEAR_n <- rep(adh$BROOD_YEAR[!is.na(adh$BROOD_YEAR)],  # repeat each element of a vector that is all the rows that have years
                        times= c(
                          diff(which(!is.na(adh$BROOD_YEAR))),  # repeat each year n times, where n=the difference between row numbers that have years
                          nrow(adh) - tail(which(!is.na(adh$BROOD_YEAR)),1) +1 # repeat the last year n times, were n=number of rows - position of last year + 1
) )

