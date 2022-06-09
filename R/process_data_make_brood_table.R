# Read in and process Nicola CWT Escapement Data - from Chuck Parken

rm(list=ls())

library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

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

hd <- hd[!hd$release_stage %in% c("E", "Z"), ]#remove egg and zygote stages
# Summarise totals for each brood year
hd_sum <- hd %>% group_by(release_stage, brood_year, stock_location_name) %>% summarise(sum_tagged_adclipped= sum(tagged_adclipped, na.rm=TRUE), sum_untagged_unclipped=sum(untagged_unclipped, na.rm=TRUE))

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
strays <- merge(adm[, c(3,4,6,27)], hd[hd$stock_location_name %in% c("S-Coldwater R", "S-Spius Cr"), ], by.x="Tagcode", by.y="tag_code_or_release_id")
# Look at strays
fig_hatchery_strays_spius_coldwater <- ggplot(strays, aes(y=number_estimated_adj_for_no_data_and_lost_pin, x=brood_year, fill=stock_location_name)) +
  geom_col(position=position_dodge()) +
  theme_bw()
fig_hatchery_strays_spius_coldwater
#ggsave("./figures/fig_hatchery_strays_spius_coldwater.png", fig_hatchery_strays_spius_coldwater )

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
# remove all rows with brood year > 2014 (need to have only years with complete record, 4 yr fish from brood year 2015 will be returning fall 2019) - TO UPDATE WITH 2019 SPAWNING SEASON DATA
adhl <- adhl[!adhl$BROOD_YEAR_n > 2014, ]
# Check to see if all tagcodes for CWT hatchery stock adults are from Nicola stock
adhl[ !(adhl$Tagcode %in% hd$tag_code_or_release_id), 'Tagcode']

# Add CWT mark recapture estimates of CWT returns and hatchery removal for broodstock CWT returns by CWT code, brood year, age at return ---------
#First trim down 
CWT_adult_comb <- merge(adms, adhl, by.x=c("Brood Year", "Spawning Year", "Tagcode"), by.y=c("BROOD_YEAR_n", "spawning_year", "Tagcode"), all=TRUE)
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
# remove all rows with brood year > 2014 (need to have only years with complete record, 4 yr fish from brood year 2015 will be returning fall 2019) - TO UPDATE WITH 2019 SPAWNING SEASON DATA
CWT_all <- CWT_all[ !CWT_all$brood_year > 2014, ]
# Remove one Coldwater row with NA returns
CWT_all <- CWT_all[!(CWT_all$stock_location_name=="S-Coldwater R" & is.na(CWT_all$sum_CWT_returns)), ]

####################################################
#### ACCOUNTING FOR UNMARKED HATCHERY RELEASES ##### ---------------
####################################################
## There are two types of untagged hatchery releases that need to be accounted for: 

# 1. Untagged releases that had tagged releases in the same release stage/brood year/stock
#     A. Nicola: 
#       i. Unmarked fry 1985-1987
#       ii. Unmarked sub-yearling smolts 1984-1985, 1987-1992
#       iii. Unmarked yearling smolts 1985-2016
#     B. Coldwater
#     C. Spius

# 2. Untagged releases that have tagged releases in another stage, where those two stages 
#     have paired tagged releases in another brood year

# 3. Untagged releases that have tagged releases in another stock, where those two stages 
#     have paired tagged releases in another brood year

# 4. Coldwater fry and Spius fry and smolts, which never had any CWT tags

# Working with hd_sum and CWT_all

master <- merge(hd_sum, CWT_all, by=c("release_stage", "brood_year", "stock_location_name", "sum_tagged_adclipped", "sum_untagged_unclipped"), all=TRUE)

# 1. 
# Get rows for type 1
case1 <- which(master[ ,"sum_tagged_adclipped"]>0 & 
                 master[ ,"sum_untagged_unclipped"]>0)
case1 


# 2. 
# Get rows for type 2
case2 <- which(master[,"stock_location_name"]=="S-Nicola R" & 
                 master[ ,"sum_tagged_adclipped"]==0 & 
                 master[ ,"sum_untagged_unclipped"]>0)
case2 

# 3. 
# Get rows for type 3 - Spius and Coldwater
case3c <- which(master[,3]=="S-Coldwater R" & 
                  master[ ,"sum_tagged_adclipped"]==0 & 
                  master[ ,"sum_untagged_unclipped"]>0 & 
                  !master[,"release_stage"]=="F")
case3s <- which(master[,3]=="S-Spius Cr" & 
                 master[ ,"sum_tagged_adclipped"]==0 & 
                 master[ ,"sum_untagged_unclipped"]>0 & 
                 master[,"release_stage"]=="Y")
case3 <- union(case3c, case3s)
case3

# Check
master[case1,]
master[case2,]
master[case3,]

###########
# Get estimated unmarked returns for case 1 ------------
###########
master$unmarked_returns1 <- 0 # new column for unmarked returns for case 1
# Calculate unmarked returns by multiplying number of tagged unclipped releases by return index of same stage, brood year, stock
master$unmarked_returns1[case1] <- round(master$sum_untagged_unclipped[case1] * master$return_index[case1],0)

###########
# Get estimated unmarked returns for case 2 --------------
###########
# first get new data frame of case 2 rows, because we need to expand it to have rows for each return age
master2 <- master[case2, !names(master) %in% c("sum_CWT_returns", "unmarked_returns1", "return_age")]
# cross all possible return ages with each row of case 2 data frame 
return_age <- c(2:5)
master2 <- crossing(master2, return_age)

# Which rows where there were tagged releases for Nicola stocks are there that are in years with no tags on other release stage?
check_overlap <- merge(master[case2, ], master[master$sum_tagged_adclipped>0 & master$stock_location_name=="S-Nicola R" & master$brood_year %in% master$brood_year[case2], ], by=c("brood_year"), all.x=TRUE) # If all.x=FALSE, no error on line 264 for loop, and output is slightly different or brood year 1996 age 3 and 4 returns (differences of 4 or 5 fish).  
check_overlap_sum <- check_overlap %>% group_by(brood_year, release_stage.x, release_stage.y) %>% summarise(n=nrow(.))
check_overlap_sum$return_index_factor <- paste0(check_overlap_sum$release_stage.x, "_", check_overlap_sum$release_stage.y)

# Calculate Return index factors ----------
# Get return index factors for life stages 
# make release stage into a column with return index as values
return_index_factors <- spread(CWT_all[ ,!names(CWT_all) %in% c("sum_tagged_adclipped", "sum_untagged_unclipped", "sum_CWT_returns")], key=release_stage, value=return_index)
##### 
# THIS PART NEEDS TRANSFORMATION / BACKTRANSFORMATION FOR 0 DENOMINATORS
# for(i in 1:nrow(return_index_factors)) {
#   return_index_factors$F_Y[i] <- 
#     ifelse(return_index_factors$Y[i]==0, log(return_index_factors$`F`[i] +1) - log(return_index_factors$Y[i]  +1),
#            return_index_factors$`F`[i] / return_index_factors$Y[i])
#   return_index_factors$S_F[i] <- 
#     ifelse(return_index_factors$`F`[i]==0, log(return_index_factors$S[i] +1) - log(return_index_factors$`F`[i]+1),
#            return_index_factors$S[i] / return_index_factors$`F`[i])
#   return_index_factors$S_Y <- 
#     ifelse(return_index_factors$Y[i]==0, log(return_index_factors$S[i] +1) - log(return_index_factors$Y[i]  +1),
#            return_index_factors$S / return_index_factors$Y )
# }
return_index_factors$F_Y <- return_index_factors$`F` / return_index_factors$Y # get return index factor for step c.
return_index_factors$S_F <- return_index_factors$S / return_index_factors$`F` # get return index factor for step b.
return_index_factors$S_Y <- return_index_factors$S / return_index_factors$Y # get return index factor for step c.

# remove rows with return index factors that are all NA
return_index_factors <- return_index_factors[!apply(is.na(return_index_factors[ ,7:9]), 1, all),]
# Check Inf values -- These don't matter because they are 1985, 1987, and 1988 (we don't have good escapement data for these brood years) and the 1992 brood doesn't need a return index factor conversion for sub-yearlings
data.frame(return_index_factors[which(return_index_factors==Inf, arr.ind=TRUE)[,1],], row.names=NULL)

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

# Expand return index for different release stage by return index factors for case 2
# number untagged unclipped * return index factor for that conversion * return index for other release stage in same year
# New column to fill in
master2$unmarked_returns2 <- 0
# fill in column with for loop
for(i in 1:nrow(master2)) {
    master2$unmarked_returns2[i] <- 
      round(
    na.exclude(
    # Number of untagged unclipped needed to be expanded
master2[i, "sum_untagged_unclipped"] * 
      # return index factor    
  return_index_factors[return_index_factors$factor_release_stages %in%
                         check_overlap_sum[check_overlap_sum$brood_year==as.numeric(master2[i, "brood_year"]), "return_index_factor" ] & 
                         return_index_factors$return_age %in% 
                         master2[i,"return_age"], "mean_return_index_factor"]  * 
      # return index for complementary release stage in same brood year
  master[ master$release_stage == check_overlap_sum$release_stage.y[check_overlap_sum$release_stage.x == master2$release_stage[i] & 
                                                                      check_overlap_sum$brood_year == master2$brood_year[i]] &
          master$brood_year == master2$brood_year[i] &
          master$return_age == master2$return_age[i]  , "return_index"]  
)[1],
0)
}

# make results that result in numeric lenght 0 values into NAs
master2$unmarked_returns2[which(lengths(master2$unmarked_returns2) == 0)] <- NA
master2$unmarked_returns2 <- unlist(master2$unmarked_returns2)


########## RIGHT NOW IT WORKS FOR EVERYHTING EXCEPT BROOD YEARS > 2014 and A COUPLE AGE 2 RETURNS THAT WE PROBABLY DON'T HAVE RETURN INDEXES FOR BECAUSE THERE WERE NO AGE 2 RETURNS FROM THE COMPLEMENTARY RELEASE STAGE ##########

#############
# Get estimated unmarked returns for case 3 --------------
#############

# get rows of just case 3
master3 <- master[case3, !names(master) %in% c("sum_CWT_returns", "unmarked_returns1", "return_age")]
# cross all possible return ages with each row of case 2 data frame 
return_age <- c(2:5)
master3 <- crossing(master3, return_age)

# Which rows where there were tagged releases for Nicola stocks are there that are in years with no tags on Coldwater and Spius stocks
check_overlap3 <- merge(master[case3, ], master[master$sum_tagged_adclipped>0 & master$stock_location_name=="S-Nicola R" & master$brood_year %in% master$brood_year[case3], ], by=c("brood_year", "release_stage"), all.x=TRUE)
check_overlap3_sum <- check_overlap3 %>% group_by(brood_year, release_stage, stock_location_name.x, stock_location_name.y ) %>% summarise(n=nrow(.))
check_overlap3_sum$return_index_factor <- paste0(check_overlap3_sum$stock_location_name.x, "_", check_overlap3_sum$stock_location_name.y)
check_overlap3_sum$return_index_factor <- gsub("S-Nicola R", "NI", check_overlap3_sum$return_index_factor)
check_overlap3_sum$return_index_factor <- gsub("S-Coldwater R", "CO", check_overlap3_sum$return_index_factor)
check_overlap3_sum$return_index_factor <- gsub("S-Spius Cr", "SP", check_overlap3_sum$return_index_factor)

# Get return index factors between stocks
# Group CWT data to get return index factors between stocks
return_index_factors_stock <- CWT_all[ ,!names(CWT_all) %in% c("sum_tagged_adclipped", "sum_untagged_unclipped", "sum_CWT_returns")] %>% 
  group_by(release_stage) %>% 
  spread(key=stock_location_name, value=return_index)
return_index_factors_stock$SP_NI <- return_index_factors_stock$`S-Spius Cr` /  return_index_factors_stock$`S-Nicola R`
return_index_factors_stock$CO_NI <- return_index_factors_stock$`S-Coldwater R` /  return_index_factors_stock$`S-Nicola R`
unique(return_index_factors_stock$release_stage)
# remove rows with return index factors that are all NA
return_index_factors_stock <- return_index_factors_stock[!apply(is.na(return_index_factors_stock[ ,6:8]), 1, all),]
# remove return index columns for stages
return_index_factors_stock <- return_index_factors_stock[ , !names(return_index_factors_stock) %in% c("S-Nicola R", "S-Coldwater R", "S-Spius Cr")]
# convert back to long format
return_index_factors_stock <- gather(return_index_factors_stock, key="factor_release_stocks", value="return_index_factor", 4:5)
# Keep only real numbers
# return_index_factors_stock <- return_index_factors_stock[ grep("[[:digit:]].*",return_index_factors_stock$return_index_factor)  , ]
# Average by factor_release_stages and age
###########
# replace Inf return index factors (that had denominator = 0) by NA -------- CHECK WITH CHUCK TO SEE IT THIS IS RIGHT
############
return_index_factors_stock$return_index_factor[return_index_factors_stock$return_index_factor==Inf] <- NA
return_index_factors_stock <- return_index_factors_stock %>% group_by(factor_release_stocks, return_age, release_stage) %>% summarise(mean_return_index_factor= mean(return_index_factor, na.rm=TRUE)) %>% as.data.frame(.)
# Remove NaN row 
return_index_factors_stock <- return_index_factors_stock[!return_index_factors_stock$mean_return_index_factor=="NaN", ]


# number untagged unclipped * return index factor for that converstion * return index for Nicola Stock in same year
# New column to fill in
master3$unmarked_returns3 <- 0

# fill in column with for loop
for(i in 1:nrow(master3)) {
  master3$unmarked_returns3[i] <- 
    round(
      na.exclude(
      # Number of untagged unclipped needed to be expanded
      master3[i, "sum_untagged_unclipped"] * 
        # return index factor    
        return_index_factors_stock[return_index_factors_stock$factor_release_stocks %in%
                                     check_overlap3_sum[check_overlap3_sum$brood_year==as.numeric(master3[i, "brood_year"]), "return_index_factor" ] & 
                                     return_index_factors_stock$return_age %in% 
                                     master3[i,"return_age"], "mean_return_index_factor"]  * 
        # return index for complementary release stage in same brood year
        master[ master$stock_location_name == check_overlap3_sum$stock_location_name.y[check_overlap3_sum$stock_location_name.x == master3$stock_location_name[i] & 
                                                                                         check_overlap3_sum$brood_year == master3$brood_year[i]] &
                  master$brood_year == master3$brood_year[i] &
                  master$return_age == master3$return_age[i] &
                  master$release_stage== master3$release_stage[i] , "return_index"]  
    )[1],
    0)
}

# make results that result in numeric lenght 0 values into NAs
master3$unmarked_returns3[which(lengths(master3$unmarked_returns3) == 0)] <- NA
master3$unmarked_returns3 <- unlist(master3$unmarked_returns3)

# Merge estimated unmarked returns by brood year and return age from each of the three cases
cols_merge <- c("release_stage", "brood_year", "stock_location_name", "sum_tagged_adclipped", "sum_untagged_unclipped", "return_index", "return_age" )
unmarked <- merge(master[case1,], merge(master2, master3, by=cols_merge, all=TRUE), by=cols_merge, all=TRUE)
str(unmarked)
# make unmarked_returns2 and unmarked_returns3 into numeric vectors
# unmarked$unmarked_returns2 <- unlist(unmarked$unmarked_returns2)

unmarked_totals <- unmarked %>% group_by(brood_year, return_age) %>% summarise(unmarked_hatchery_returns= sum(unmarked_returns1, unmarked_returns2, unmarked_returns3, na.rm=TRUE))

# # make a data frame summarized by run year to use for 1992-1994 to correct for unmarked hatchery adults ( we don't have age-specific escapement data for these years)
# unmarked_total_sum <- unmarked_totals 
# unmarked_total_sum$run_year <- unmarked_total_sum$brood_year + unmarked_total_sum$return_age
# unmarked_total_sum1 <- unmarked_total_sum %>% group_by(run_year) %>% summarise(unmarked_hatchery_returns_all_ages = sum(unmarked_hatchery_returns))

# save a csv 
write.csv(unmarked_total_sum1, "./data_out/unmarked_hatchery_returns_by_year.csv", row.names=FALSE)

# Read in brood table of escapement ----------  CHECK THAT THIS WORKS WITH LATEST DATA
escapement <- read_excel("./data/Nicola (1995-2018) RiverSpawnerplusHatcheryEscbyAge and Clip status Nov 5 2019.xlsx", sheet=2, skip=2 )
names(escapement)[1:3] <- c("run_year", "return_age", "brood_year")
escapement[,5:10] <- round(escapement[,5:10],0) # round to nearest whole number
# merge with unmarked totals
escapement1 <- merge(escapement, unmarked_totals, by=c("return_age", "brood_year"), all.x=TRUE)
# Subtract estimated unclipped hatchery returns from unclipped returns to get 'true' wild recruits
escapement1$wild_escapement <- escapement1$Corrected_Unclipped_Nicola_Escapement - escapement1$unmarked_hatchery_returns
# For negative values, make into 0 ------- CHECK WITH CHUCK TO SEE IF THIS IS OKAY
 for(i in 1:nrow(escapement1)) {
   escapement1$wild_escapement[i] <-ifelse(escapement1$wild_escapement[i]>=0,
                                                   escapement1$wild_escapement[i],
                                                   0)
 }

# # Make table that has more info for supplemental
# escape_sum <- escapement1 %>% group_by(run_year) %>% summarise_at(.vars=c("unmarked_hatchery_returns", "wild_escapement"), .funs=sum, na.rm=TRUE)
# escape_sum$ratio = round(escape_sum$unmarked_hatchery_returns/escape_sum$wild_escapement,2)

# Get total spawners (wild + hatchery) for each run year
escapement1$total_spawners <- escapement1$Corrected_Unclipped_River_Spawners + escapement1$Clipped_River_Spawners
# Get true wild spawners
# First need to get percentage of true wild escapement from unclipped escapement, then apply percentage to unmarked spawners
escapement1$perc_true_wild_escapement <- escapement1$wild_escapement / escapement1$Corrected_Unclipped_Nicola_Escapement
# Replace NaN with 0
escapement1$perc_true_wild_escapement[escapement1$perc_true_wild_escapement=="NaN"] <- 0
# Get true wild spawners
escapement1$true_wild_spawners <- round(escapement1$Corrected_Unclipped_River_Spawners * escapement1$perc_true_wild_escapement,0)
# Get true hatchery spawners
escapement1$true_hatchery_spawners <- escapement1$total_spawners - escapement1$true_wild_spawners


# make summary of spawner data (which will have more years than recruitment, which will be limited by exploitation + years without full age returns)
spawners_sum <- escapement1 %>% group_by(run_year) %>% summarise_at(.vars=c("total_spawners", "true_wild_spawners", "true_hatchery_spawners"), .funs=sum, na.rm=TRUE)
names(spawners_sum) <- sub("true_", "", names(spawners_sum))
# write to csv
write.csv(spawners_sum, "./data_out/spawners_1995-2018.csv", row.names=FALSE)

# visual check
plot(escapement1$Corrected_Unclipped_Nicola_Escapement)
points(escapement1$wild_escapement, col="dodgerblue")
points(escapement1$true_wild_spawners, col="orange")
abline(h=0)

# Account for exploitation rate
# Read in exploitation rate data
exploit1 <- read_excel("./data/Nicola ER estimates (BY 1985-2013).xlsx", range=c("A5:D36"))
exploit2 <- read_excel("./data/Nicola ER estimates (BY 1985-2013).xlsx", range=c("A5:G36"))
exploit2 <- exploit2[ ,c(1,5:7)]
names(exploit2)[2:4] <-c("3", "4", "5") 
exploit3 <- read_excel("./data/Nicola ER estimates (BY 1985-2013).xlsx", range=c("A5:J36"))
exploit3 <- exploit3[ ,c(1,8:10)]
names(exploit3)[2:4] <-c("3", "4", "5") 
# wide to long format
exploit1l <- exploit1 %>% gather(key="return_age", value="sum_TMAEQ_tot", 2:4)
exploit2l <- exploit2 %>% gather(key="return_age", value="sum_escapement", 2:4)
exploit3l <- exploit3 %>% gather(key="return_age", value="exploitation_rate", 2:4)
# combine three data frames into a list
exploit_list <- list(exploit1l, exploit2l, exploit3l)
# rename brood year column
exploit_list <- lapply(exploit_list, function(i) {
  names(i)[1] <- "brood_year"
  return(i)
})
# combine the three data frames                       
exploit <- exploit_list %>% purrr::reduce(full_join, by=c("brood_year", "return_age"))
# remove brood year > 2013 because we don't have full data
#exploit <- exploit[!exploit$brood_year>2013, ]

# Try a different approach - get ratios of wild to CWT fish for each cohort
# First check the age distribution for hatchery and wild fish
matH <- escapement1 %>% 
  filter(brood_year >=1992 & brood_year <= 2014 ) %>%
  group_by(brood_year, return_age) %>%  
  summarise(n = sum(Clipped_Nicola_Escapement, unmarked_hatchery_returns)) %>%
  mutate(freq = n / sum(n)) %>% 
  ggplot(data=., aes(y=freq, x=brood_year, fill=factor(return_age))) +
  ggtitle("Hatchery") +
  geom_col()

matW <- escapement1 %>% 
  filter(brood_year >=1992 & brood_year <= 2014 ) %>%
  group_by(brood_year, return_age) %>%  
  summarise(n = sum(wild_escapement)) %>%
  mutate(freq = n / sum(n)) %>% 
  ggplot(data=., aes(y=freq, x=brood_year, fill=factor(return_age))) +
  ggtitle("Wild") +
  geom_col()
#ggsave("./figures/fig_mat_schedule.png", grid.arrange(matH, matW, ncol=1))


# For age 3 and 5 returns, use average of other years for NA years
# Get averages of years that don't have NA or 1.0 for exploitation rate
 exploit_sum <- exploit %>% filter(!is.na(exploitation_rate), !exploitation_rate==1) %>% 
   group_by(return_age) %>% summarise(mean_exploitation_rate=mean(exploitation_rate, na.rm=TRUE)) # make summary data frame
 exploit1 <- exploit
 # For ages/brood years with 0 fisheries mortality and 0 escapement, use average escapement
 for(i in 1:nrow(exploit1)) { 
   exploit1$exploitation_rate[i] <- unlist(ifelse(exploit1$sum_TMAEQ_tot[i]==0 & exploit1$sum_escapement[i]==0, 
                                         exploit_sum[exploit_sum$return_age ==exploit1$return_age[i], "mean_exploitation_rate"],
                                         exploit1$exploitation_rate[i]
   ))
}
which(is.na(exploit1$exploitation_rate))  # check
str(exploit1)

# For years with CWT recoveries in the fishery but not in escapement, use expansion factor between wild and CWT fish of the other life stages

# Replace 100% exploitation rate exploitation rate from expansion factor
for(i in 1:nrow(exploit1)) { 
  exploit1$exploitation_rate[i] <- unlist(ifelse(exploit1$sum_TMAEQ_tot[i]>0 & exploit1$sum_escapement[i]==0, # does row have fisheries mortality but no escapement for CWT
      exploit1$sum_TMAEQ_tot[i] *  # if true, multiply CWT fisheries mort for age 3 by
        sum(escapement1$wild_escapement[escapement1$brood_year==exploit1$brood_year[i] & escapement1$return_age %in% c(4,5)]) / #expansion factor to get wild fishery mortality, which is wild escapement of ages 4 + 5 divided by
              sum(escapement1$true_hatchery_spawners[escapement1$brood_year==exploit1$brood_year[i] & escapement1$return_age %in% c(4,5)]) / #hatchery escapement for age 4 and 5
           
        # this is to get estimated wild fishery mortality
        (exploit1$sum_TMAEQ_tot[i] *  # if true, multiply CWT fisheries mort for age 3 by
        sum(escapement1$wild_escapement[escapement1$brood_year==exploit1$brood_year[i] & escapement1$return_age %in% c(4,5)]) / #expansion factor to get wild fishery mortality, which is wild escapement of ages 4 + 5 divided by
        sum(escapement1$true_hatchery_spawners[escapement1$brood_year==exploit1$brood_year[i] & escapement1$return_age %in% c(4,5)]) + #then divide by sum of wild fishery mortality and wild escapement to get exploitation rate
          escapement1$wild_escapement[escapement1$brood_year==exploit1$brood_year[i] & escapement1$return_age==3]),       # and add wild spawners to wild fishery mortality
                                                 
      exploit1$exploitation_rate[i] # if false, just use existing exploitation rate
  ))
}

# NOTE - NO EXPLOITATION RATE FOR BROOD YEAR 2014

str(exploit1)
# visual check
ggplot(exploit1, aes(y=exploitation_rate, x=brood_year, colour=return_age)) +
  geom_point(aes(size=sum_escapement)) +
  geom_line() +
  #geom_line(aes(y=sum_TMAEQ_tot, x=brood_year)) +
  #geom_line(aes(y=sum_escapement, x=brood_year)) +
  theme_bw()

# Apply exploitation to unclipped escapement
escapement2 <- merge(escapement1, exploit1[,c(1,2,5)], by=c("return_age", "brood_year"), all.x=TRUE)
# calculate recruits from escapement and exploitation rate
escapement2$recruits <- round(escapement2$wild_escapement / (1 - escapement2$exploitation_rate),0)
# remove NA rows, mostly age 2 fish
escapement2 <- escapement2[!is.na(escapement2$recruits), ]
# select brood years with complete recruits (ages 3-5)
# escapement2 <- escapement2[escapement2$brood_year > 1991 & escapement2$brood_year < 2014, ]

#visual check 
ggplot(escapement2, aes(y=recruits, x=brood_year)) + 
  geom_point() +
  geom_path() +
  geom_point(aes(y=Corrected_Unclipped_Nicola_Escapement ), colour="gray") +
  geom_point(aes(y=Corrected_Unclipped_Nicola_Escapement- unmarked_hatchery_returns, x=brood_year + rep(0.2, nrow(escapement2))), colour="orange") +
  geom_point(aes(y=wild_escapement), colour="blue") +
  geom_point(aes(y=true_wild_spawners), colour="red") +
  scale_x_continuous(breaks=seq(min(escapement2$brood_year), max(escapement2$brood_year),1)) +
  facet_grid(~return_age) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) 

# Write csv of brood table 
write.csv(escapement2[,c("return_age", "run_year", "brood_year", "recruits", "total_spawners", "true_wild_spawners", "true_hatchery_spawners")], "./data_out/nicola_brood_table.csv", row.names=FALSE)

################################
############# FIGURES ##########
################################

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

# Look at release for all years and stocks
hd_sum %>% gather(key=tag_type, value=count, sum_tagged_adclipped, sum_untagged_unclipped) %>% 
  ggplot(aes(x=brood_year, y=count, fill=tag_type)) +
  geom_col() +
  guides(fill=FALSE) +
  facet_grid(release_stage~stock_location_name) +
  theme_bw()
ggsave("./figures/fig_releases_by_stock_and_tag.png", plot=last_plot(), width=6, height=4)

# Look at returns for all years and stocks
ggplot(CWT_all, aes(y=sum_CWT_returns, x=brood_year)) +
    geom_col() +
    facet_grid(release_stage~stock_location_name) +
    theme_bw()
ggsave("./figures/fig_returns_by_stock_and_stage.png", plot=last_plot(), width=6, height=4)
