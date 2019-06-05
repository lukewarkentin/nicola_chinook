# Read in and process Nicola CWT Escapement Data - from Chuck Parken

rm(list=ls())
setwd("D:/22_masters/100.002_Nicola-Chinook-cohorts/10_R/nicola_chinook")

library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

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

#Get one count column - wide to long format
#to_gather <- tail(names(hd), n=5) #get last 5 column names - these hold the count data
#hdl <- gather(hd, key="tag_mark_type", value="fish_count", to_gather)
# Trim to just desired columns for merging
names(hd)
to_keep <- names(hd)[c(1,2,5,6,7,11,15,16,31,32,34,36:40)]
to_keep
hd_to_merge <- hd[ , to_keep]

# Adult CWT Returns - Mark-recapture program ----------
adm <- read_excel("./data/Nicola R  Escapement CWT Data 1987-2018.xlsx", sheet = "Nicola - Escapement CWT Data")
# rename the estimated number of CWT fish column 
names(adm)[27] <- "number_estimated_adj_for_no_data_and_lost_pin"
adm <- adm[!is.na(adm$River), ] #remove blank rows

# combine male and female returns for each brood year, spawn year,and CWT code
adms <- adm %>% group_by(`Brood Year`, `Spawning Year`, Tagcode) %>% summarise(estimated_CWT_returns_adj = sum(number_estimated_adj_for_no_data_and_lost_pin, na.rm=TRUE))

# Adult CWT Returns - Take by hatchery for brood stock ----------
adh <- read_excel("./data/Hatchery Removals by Nicola CWT code.xlsx", skip=4)
adh <- adh[-grep("Total", adh$BROOD_YEAR), -grep("Grand Total", names(adh))] # remove rows with yearly totals and column of grand total
head(adh)

# fill in missing years
adh$BROOD_YEAR_n <- rep(adh$BROOD_YEAR[!is.na(adh$BROOD_YEAR)],  # repeat each element of a vector that is all the rows that have years
                        times= c(
                          diff(which(!is.na(adh$BROOD_YEAR))),  # repeat each year n times, where n=the difference between row numbers that have years
                          nrow(adh) - tail(which(!is.na(adh$BROOD_YEAR)),1) +1 # repeat the last year n times, were n=number of rows - position of last year + 1
) )
adh <- adh[ ,-1] # remove obsolete BROOD_YEAR column
# Gather data from ages of fish into age column and count column (wide to long format)
to_gather2 <- as.character(2:5) #make vector of column names to gather (ages at return columns)
# replace NA in count cells with zeros
adh[is.na(adh)] <- 0

adhl <- gather(adh, key="return_age", value="hatchery_removals_broodstock", to_gather2 )
adhl <- adhl[!is.na(adhl$hatchery_removals_broodstock), ] # remove NA counts
# make brood year and return age into integers
adhl$BROOD_YEAR_n <- as.integer(adhl$BROOD_YEAR_n)
adhl$return_age <- as.integer(adhl$return_age)
# add spawning year column
adhl$spawning_year <- adhl$BROOD_YEAR_n + adhl$return_age

# Add CWT mark recapture estimates of CWT returns and hatchery removal for broodstock CWT returns by CWT code, brood year, age at return ---------
#First trim down 
CWT_adult_comb <- merge(adms, adhl, by.x=c("Brood Year", "Spawning Year", "Tagcode"), by.y=c("BROOD_YEAR_n", "spawning_year", "Tagcode" ), all=TRUE)
names(CWT_adult_comb)[grep("Brood Year", names(CWT_adult_comb))] <- "brood_year_adult_data"

# Merge adult data with hatchery release CWT data ------------
CWT_all <- merge(hd_to_merge, CWT_adult_comb, by.x=c("tag_code_or_release_id"), by.y="Tagcode", all=TRUE)
CWT_all <- CWT_all[ ,-grep("brood_year_adult_data", names(CWT_all))] # remove brood year from adult data
CWT_all$return_age <- CWT_all$`Spawning Year` - CWT_all$brood_year
CWT_all$estimated_CWT_returns_adj[is.na(CWT_all$estimated_CWT_returns_adj)] <- 0 #replace NAs with 0
CWT_all$hatchery_removals_broodstock[is.na(CWT_all$hatchery_removals_broodstock)] <- 0 #replace NAs with 0
CWT_all$CWT_total_returns <- round(CWT_all$estimated_CWT_returns_adj + CWT_all$hatchery_removals_broodstock, 0)
CWT_all$return_index <- CWT_all$CWT_total_returns/CWT_all$tagged_adclipped

# Look at just fry releases
CWT_sub <- CWT_all[ CWT_all$release_stage %in% "F" & CWT_all$brood_year %in% 1987, ]

# Calculate Return index factors ----------
#calulate average return index for each brood year and return age, across CWT tag codes
CWT_average <- CWT_all %>% group_by(brood_year, return_age, `Spawning Year`, release_stage) %>% summarise(mean_return_index = mean(return_index, na.rm=TRUE), sd_return_index = sd(return_index, na.rm=TRUE))
# make releas stage into a column with return index as values
CWT_average_spread <- spread(CWT_average[ ,-grep("sd_return_index", names(CWT_average))], key=release_stage, value=mean_return_index)
#select only years 1985 and 1987, which have CWT applications to fry and yearling smolts
CWT_average_spread_sub <- CWT_average_spread[CWT_average_spread$brood_year %in% c(1985, 1987),]
# Calculate return index factor, which is the return index of the fry / return index of yearling smolts
CWT_average_spread_sub$return_index_factor_F_Y <- CWT_average_spread_sub$`F` / CWT_average_spread_sub$Y
CWT_average_spread_sub$return_index_factor_F_Y <- as.numeric(sub("NaN", 0, CWT_average_spread_sub$return_index_factor_F_Y ))
# average return index for each return age (averae across 1985 and 1987 brood years for each return age)
CWT_return_index_factors <- CWT_average_spread_sub %>% group_by(return_age) %>% summarise(mean_return_index_factor_F_Y= mean(return_index_factor_F_Y, na.rm=TRUE))

# Expand unmarked fry using return index factors and return indices for tagged smolts
years_tagged_fry <- c(1985:1987)
years_fry <- unique(CWT_all[CWT_all$release_stage %in% "F", grep("brood_year", names(CWT_all))])
years_untagged_fry <- sort(setdiff( years_fry, years_tagged_fry))
return_ages <- 2:5
adults_from_unmarked_fry <- expand.grid(brood_year=years_untagged_fry, return_age = return_ages)

calc_adults_from_unmarked_fry <- function(x) { # function where x=data frame, i= year and j =return age
  CWT_average_spread$Y[CWT_average_spread$brood_year %in% x[1] & CWT_average_spread$return_age %in% x[2]] * # Return index for smolts
  CWT_return_index_factors$mean_return_index_factor_F_Y[CWT_return_index_factors$return_age==x[2]] * # Return Index factor for fry
  sum(CWT_all$untagged_unclipped[CWT_all$release_stage=="F" & CWT_all$brood_year==x[1]], na.rm=TRUE ) # fry unclipped released
}

adults_from_unmarked_fry$count <-  apply(adults_from_unmarked_fry, 1, calc_adults_from_unmarked_fry)

# for (i in years_untagged_fry) {
#  for(j in return_ages) {
 #    CWT_average_spread$S[CWT_average_spread$brood_year==i & CWT_average_spread$return_age==j] * # Return index for smolts
#                                      CWT_return_index_factors$mean_return_index_factor_F_Y[CWT_return_index_factors$return_age==j] # Return Index factor for fry
#                                      sum(CWT_all$untagged_unclipped[CWT_all$release_stage=="F" & CWT_all$return_age==j & CWT_all$brood_year==i] ) # fry unclipped released
    
#  }
#}

# Examine data -----------
ggplot(CWT_all[CWT_all$release_stage %in% c("F", "S", "Y"),], aes(y=return_index, x=brood_year, colour=factor(return_age), fill=factor(return_age))) +
  stat_summary(fun.y="mean", na.rm=TRUE, geom="point", size=3, colour="black", shape=23) +
  geom_point(shape=1) +
  facet_grid(release_stage~., scales="free_y") +
  theme_bw()
