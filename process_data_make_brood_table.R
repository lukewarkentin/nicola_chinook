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
hd_complete <- read.csv("./data/RMIS-chinook-CWT-hatchery-release-data.txt", stringsAsFactors = FALSE)
# Process data - only want Nicola, Coldwater, and Spius stocks
# remove Bonaparte and Deadman
# d <- d[ which(d$stock_location_name %in% c("S-Bonaparte R", "S-Deadman R") == FALSE), ]
hd_complete[!hd_complete$hatchery_location_name=="H-Spius Creek H",] # check releases from other sources
# Merritt School released 80 untagged/unclipped 0+ smolts in 1993 and 80 untagged/unclipped fry in 1995. 
# Nicola Inc. released 18000 untagged/unclipped eggs in 1981 and 2500 untagged/unclipped eggs in 1982.
# I think it's safe to remove these rows.
# only include spius creek 
hd <- hd_complete[hd_complete$hatchery_location_name =="H-Spius Creek H",]
# Include only Nicola Stock for analysis
hd <- hd[hd$stock_location_name %in% c("S-Nicola R","S-Coldwater R", "S-Spius Cr"), ]

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
hd_to_merge <- hd[ , to_keep] #obsolete
hd_sum <- hd %>% group_by(release_stage, brood_year, stock_location_name) %>% summarise(sum_tagged_adclipped= sum(tagged_adclipped, na.rm=TRUE))
  
# Adult CWT Returns - Mark-recapture program ----------
adm <- read_excel("./data/Nicola R  Escapement CWT Data 1987-2018.xlsx", sheet = "Nicola - Escapement CWT Data")
# rename the estimated number of CWT fish column 
names(adm)[27] <- "number_estimated_adj_for_no_data_and_lost_pin"
adm <- adm[!is.na(adm$River), ] #remove blank rows

###################
# Remove no pin, lost pin, no data rows  ----- CHECK WITH CHUCK PARKEN THAT THIS IS OKAY - YES**********
adm <- adm[grep("^[[:digit:]].*", adm$Tagcode), ]
##################
unique(adm$`Brood Year`)
# some rows are missing brood year but have tag code. add brood year for these rows
for(i in 1:nrow(adm)) {
  adm$`Brood Year`[i] <- ifelse(is.na(adm$`Brood Year`[i]), 
         hd$brood_year[match(adm$Tagcode[i], hd$tag_code_or_release_id)],
         adm$`Brood Year`[i])
}

# Some of the CWT returns are of Coldwater and Spius stock
strays_hd <- hd_complete[ hd_complete$tag_code_or_release_id %in% adm$Tagcode[is.na(adm$`Brood Year`)], ]
strays <- merge(adm[ is.na(adm$`Brood Year`),c(3,4,6,27)], strays_hd, by.x="Tagcode", by.y="tag_code_or_release_id")
# Look at strays
fig_hatchery_strays_spius_coldwater <- ggplot(strays, aes(y=number_estimated_adj_for_no_data_and_lost_pin, x=brood_year, fill=stock_location_name)) +
  geom_col(position=position_dodge()) +
  theme_bw()
fig_hatchery_strays_spius_coldwater
ggsave("./figures/fig_hatchery_strays_spius_coldwater.png", fig_hatchery_strays_spius_coldwater )

# combine male and female returns for each brood year, spawn year,and CWT code
adms <- adm %>% group_by(`Brood Year`, `Spawning Year`, Tagcode) %>% summarise(estimated_CWT_returns_adj = sum(number_estimated_adj_for_no_data_and_lost_pin, na.rm=TRUE))
tail(adms)

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
# remove all rows with brood year > 2014 (need to have only years with complete record, 4 yr fish from brood year 2015 will be returning fall 2019) - TO UPDATE WITH 2019 SPAWNIN SEASON DATA
adhl <- adhl[!adhl$BROOD_YEAR_n > 2014, ]
# Check to see if all tagcodes for CWT hatchery stock adults are from Nicola stock
adhl[ !(adhl$Tagcode %in% hd$tag_code_or_release_id), 'Tagcode']

# Add CWT mark recapture estimates of CWT returns and hatchery removal for broodstock CWT returns by CWT code, brood year, age at return ---------
#First trim down 
CWT_adult_comb <- merge(adms, adhl, by.x=c("Brood Year", "Spawning Year", "Tagcode"), by.y=c("BROOD_YEAR_n", "spawning_year", "Tagcode" ), all=TRUE)
# change name of brood year column
names(CWT_adult_comb)[grep("Brood Year", names(CWT_adult_comb))] <- "brood_year"
# add return age if missing
for (i in 1:nrow(CWT_adult_comb)) {
  CWT_adult_comb$return_age[i] <- ifelse(is.na(CWT_adult_comb$return_age[i]), CWT_adult_comb$`Spawning Year`[i]-CWT_adult_comb$brood_year[i], CWT_adult_comb$return_age[i] )
}
CWT_adult_comb$estimated_CWT_returns_adj[is.na(CWT_adult_comb$estimated_CWT_returns_adj)] <- 0 #replace NAs with 0
CWT_adult_comb$hatchery_removals_broodstock[is.na(CWT_adult_comb$hatchery_removals_broodstock)] <- 0 #replace NAs with 0
CWT_adult_comb$CWT_total_returns <- round(CWT_adult_comb$estimated_CWT_returns_adj + CWT_adult_comb$hatchery_removals_broodstock, 0) # add CWT returns from mark-recapture and hatchery broodstock
# Check that all CWT returns are associated with Nicola, Spius, or Coldwater stocks and merge with release data
CWT_adult_comb_stock <- merge(CWT_adult_comb, hd_complete[ ,names(hd_complete) %in% c("stock_location_name", "tag_code_or_release_id", "release_stage")], by.x="Tagcode", by.y="tag_code_or_release_id", all.x=TRUE)

# Summarise totals for each brood year and return age and release stage and stock
CWT_adult_sum <- CWT_adult_comb_stock %>% group_by(brood_year, return_age, release_stage, stock_location_name) %>% summarise(sum_CWT_returns= sum(CWT_total_returns, na.rm=TRUE))

# Merge adult data with hatchery release CWT data and calculate return index for each code------------
CWT_all <- merge(hd_sum, CWT_adult_sum, by=c("brood_year", "release_stage", "stock_location_name"), all=TRUE)
# check that worked
#check <- CWT_all %>% group_by(stock_location_name, brood_year, release_stage) %>% summarise(released = paste(sum_tagged_adclipped, collapse=", "))
# Calculate return index
CWT_all$return_index <- CWT_all$sum_CWT_returns/CWT_all$sum_tagged_adclipped # get return index by dividing adults / releases for each CWT code
# Remove na brood year row (with ~55 returns; possibly other stock) and Z and E release stage rows
CWT_all <- CWT_all[ !is.na(CWT_all$brood_year),]
CWT_all <- CWT_all[ !CWT_all$release_stage %in% c("E", "Z"), ]
# Remove rows with 0 tagged_adclipped - not used for return index
CWT_all <- CWT_all[ !CWT_all$sum_tagged_adclipped==0, ]
# remove all rows with brood year > 2014 (need to have only years with complete record, 4 yr fish from brood year 2015 will be returning fall 2019) - TO UPDATE WITH 2019 SPAWNIN SEASON DATA
CWT_all <- CWT_all[ !CWT_all$brood_year > 2014, ]
# Remove one Coldwater row with NA returns
CWT_all <- CWT_all[!(CWT_all$stock_location_name=="S-Coldwater R" & is.na(CWT_all$sum_CWT_returns)), ]
####################################################
#### ACCOUNTING FOR UNMARKED HATCHERY RELEASES ##### ---------------
####################################################

# There are 3 main sources (14) steps total) of unmarked hatchery adult returns that need to be accounted for:
# I. Brood years with no CWT tags applied to a release stage (e.g., all fry released in 1996 were unmarked and unclipped)
#       a. Unmarked fry 1996-2017, which need to be expanded from marked yearling smolts using return index factor from 1985 and 1987
#       b. Unmarked sub-yearling smolts 1986, which need to be expanded from marked fry using return index factor from 1985 and 1987
#       c. Unmarked sub-yearling smolts 1993 and 1997, which need to be expanded from marked yearling smolts using return index factor from 1985 and 1987-1992
# II. Brood years with CWT marked and unmarked fish within same release stage (e.g., some marked fry, some unmarked fry)
#       d. Unmarked fry 1985-1987
#       e. Unmarked sub-yearling smolts 1984-1985, 19870-1992
#       f. Unmarked yearling smolts 1985-2016
# III. Strays from Coldwater and Spius hatchery stock
#       g. Unmarked Coldwater fry
#       h. Unmarked Spius fry
#       i. Unmarked Coldwater sub-yearlings
#       j. Unmarked Spius sub-yearlings
#       k. Unmarked Coldwater yearlings (CWT and non-CWT years)
#       l. Unmarked Spius yearlings (CWT and non-CWT years)

# Below are the 12 steps:

# Calculate Return index factors ----------
# Get return index factors for life stages 
# make release stage into a column with return index as values
return_index_factors <- spread(CWT_all[ ,!names(CWT_all) %in% c("sum_tagged_adclipped", "sum_CWT_returns")], key=release_stage, value=return_index)
##### 
# THIS PART NEEDS TRANSFORMATION / BACKTRANSFORMATION FOR 0 DENOMINATORS
return_index_factors$F_Y <- return_index_factors$`F` / return_index_factors$Y # get return index factor for step a.
return_index_factors$S_F <- return_index_factors$S / return_index_factors$`F` # get return index factor for step b.
return_index_factors$S_Y <- return_index_factors$S / return_index_factors$Y # get return index factor for step c.
# remove rows with return index factors that are all NA
return_index_factors <- return_index_factors[!apply(is.na(return_index_factors[ ,7:9]), 1, all),]
# remove return index columns for stages
return_index_factors <- return_index_factors[ , !names(return_index_factors) %in% c("E", "F", "S", "Y")]
# convert back to long format
return_index_factors <- gather(return_index_factors, key="factor_release_stages", value="return_index_factor", 4:6)
# Keep only real numbers
# return_index_factors <- return_index_factors[ grep("[[:digit:]].*",return_index_factors$return_index_factor)  , ]
# Average by factor_release_stages and age
###########
# replace Inf return index factors (that had denominator = 0) by NA -------- CHECK WITH CHUCK TO SEE IT THIS IS RIGHT
############
return_index_factors$return_index_factor[return_index_factors$return_index_factor==Inf] <- NA
return_index_factors <- return_index_factors %>% group_by(factor_release_stages, return_age) %>% summarise(mean_return_index_factor= mean(return_index_factor, na.rm=TRUE)) %>% as.data.frame(.)
str(return_index_factors)
# visual check
ggplot(return_index_factors, aes(y=mean_return_index_factor, x=return_age, colour=factor_release_stages )) +
  geom_point(size=3) +
  theme_bw()

# Get years that need to be accounted for
# a. Unmarked fry 1996-2017, which need to be expanded from marked yearling smolts using return index factor from 1985 and 1987
F_Y_yrs <- 
  sort(setdiff(
    unique(hd$brood_year[which(hd$untagged_unclipped>0 & hd$release_stage=="F" )]),# years with releases with untagged fry
    unique(hd$brood_year[which(hd$tagged_adclipped>0 & hd$release_stage=="F")] ))) # years with releases with adclipped fry

# b. Unmarked sub-yearling smolts 1986, which need to be expanded from marked fry using return index factor from 1985 and 1987
S_F_yrs <- 1986
# c. Unmarked sub-yearling smolts 1993 and 1997, which need to be expanded from marked yearling smolts using return index factor from 1985 and 1987-1992
S_Y_yrs <- c(1993, 1997)

# Make a data frame to fill in with unmarked adult columns
yrs <- unique(hd$brood_year)
ages <- 2:5
unmarked_adults <- expand.grid(brood_year = yrs, return_age = ages)
unmarked_adults$use_factor <- as.character(NA)
# check to make sure there is no overlap in years for factors
intersect(F_Y_yrs, S_F_yrs)
intersect(F_Y_yrs, S_Y_yrs)
intersect(S_F_yrs, S_Y_yrs)
# Fill 
unmarked_adults$use_factor[unmarked_adults$brood_year %in% F_Y_yrs] <- "F_Y"
unmarked_adults$use_factor[unmarked_adults$brood_year %in% S_F_yrs] <- "S_F"
unmarked_adults$use_factor[unmarked_adults$brood_year %in% S_Y_yrs] <- "S_Y"




# Expand unmarked fry using return index factors and return indices for tagged smolts
# NEED TO WRITE NEW FUNCTION
# Write function that can be passed in factor, year, and age
adults_from_unmarked <- function(factor, year, age)   
  
   
# Function to get unmarked adults
calc_adults_from_unmarked_fry <- function(x) { # function where x=data frame, i= year and j =return age
  CWT_average_spread$Y[CWT_average_spread$brood_year %in% x[1] & CWT_average_spread$return_age %in% x[2]] * # Return index for smolts
  CWT_return_index_factors$mean_return_index_factor_F_Y[CWT_return_index_factors$return_age==x[2]] * # Return Index factor for fry
  sum(CWT_all$untagged_unclipped[CWT_all$release_stage=="F" & CWT_all$brood_year==x[1]], na.rm=TRUE ) # fry unclipped released
}
# Apply function to data frame of years and ages
adults_from_unmarked_fry$count <-  as.numeric(apply(adults_from_unmarked_fry, 1, calc_adults_from_unmarked_fry))
str(adults_from_unmarked_fry)

# visual check 
ggplot(adults_from_unmarked_fry, aes(y=count, x=brood_year, colour=factor(return_age))) +
  geom_point()

# Examine data -----------
ggplot(CWT_all[CWT_all$stock_location_name=="S-Nicola R",], aes(y=return_index, x=brood_year, colour=factor(return_age), fill=factor(return_age))) +
  #stat_summary(fun.y="mean", na.rm=TRUE, geom="point", size=5, shape=45, stroke=5) +
  geom_point(shape=1,stroke=2, size=3) +
  facet_grid(release_stage~., scales="free_y") +
  theme_bw()
ggsave("./figures/fig_return_index_by_release_stage_broodyear_age.png", plot=last_plot(), width=6, height=4)

# Look at fry return index and sample size
ggplot(CWT_all[CWT_all$release_stage %in% "F",], aes(y=return_index, x=brood_year, colour=factor(return_age), fill=factor(return_age))) +
  geom_point(shape=1, size=2) +
  geom_text(aes(label=tagged_adclipped)) +
  theme_bw()

# Look at sub-yearling return index and sample size
ggplot(CWT_all[CWT_all$release_stage %in% "S",], aes(y=return_index, x=brood_year, colour=factor(return_age), fill=factor(return_age))) +
  geom_point(shape=1, size=2) +
  geom_text(aes(label=tagged_adclipped)) +
  theme_bw()

# Look at yearling return index and sample size
ggplot(CWT_all[CWT_all$release_stage %in% "Y",], aes(y=return_index, x=brood_year, colour=factor(return_age), fill=factor(return_age))) +
  geom_point(shape=1, size=2) +
  geom_text(aes(label=tagged_adclipped)) +
  theme_bw()

# Look at release size for fry
range(CWT_all$brood_year)
ggplot(CWT_all[CWT_all$release_stage %in% "F",], aes(y=avg_weight, x=brood_year, colour=tag_code_or_release_id, size=tagged_adclipped)) +
  geom_point(shape=1, position=position_dodge(width=0.3)) +
  guides(colour = "none") +
  scale_x_discrete(limits=seq(1984,2017,1), breaks=seq(1984,2017,1), labels=seq(1984,2017,1))+
  #geom_text(aes(label=tagged_adclipped)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# Look at release size for sub-yearlings
# Note that 1989 and 1990 had differences between CWT cohorts
ggplot(CWT_all[CWT_all$release_stage %in% "S",], aes(y=avg_weight, x=brood_year, colour=tag_code_or_release_id, size=tagged_adclipped)) +
  geom_point(shape=1, position=position_dodge(width=0.3)) +
  guides(colour = "none") +
  scale_x_discrete(limits=seq(1984,2017,1), breaks=seq(1984,2017,1), labels=seq(1984,2017,1))+
  #geom_text(aes(label=tagged_adclipped)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))

# Look at release size for yearlings
# 1993-1997 have some differences between CWT batches within the brood year
ggplot(CWT_all[CWT_all$release_stage %in% "Y",], aes(y=avg_weight, x=brood_year, colour=tag_code_or_release_id, size=yday(last_release_date))) +
  geom_point(shape=1, position=position_dodge(width=0.3)) +
  guides(colour = "none") +
  scale_x_discrete(limits=seq(1984,2017,1), breaks=seq(1984,2017,1), labels=seq(1984,2017,1))+
  #geom_text(aes(label=tagged_adclipped)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5))
