# This script uses the csv export from a shapefile, which is the intersect between two layers:  
#1. Harvested Areas of BC (Consolidated Cutblocks) clipped to the Nicola watershed (downloaded from BC Data catalogue https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-)
# and 
# 2. The named sub-watersheds within the Nicola watershed (from FWA_NAMED_WATERSHEDS_POLY from the Prpovincial freshwater atlas, https://catalogue.data.gov.bc.ca/dataset/freshwater-atlas-named-watersheds )

library(ggplot2)
library(dplyr)
library(zoo)
library(ggforce)

rm(list=ls())
# read in csv
# Notes about data:
# 1. Each row is a harvest patch, joined with data from the watershed it is located within
# 2. Since the watersheds are nested, (e.g., Nicola River, contains Guichon Creek), the same harvest patch can be found in multiple lines of the csv, once for each subcatchment it is found in
# 3. AREA_HA is the watershed area that each harvest patch is found in
# 4. areaCut_ha is the area of the harvested patch found within each watershed.

dat1 <- read.csv("./data/nicola-watersheds-cut-blocks-intersect.csv", stringsAsFactors = FALSE)
names(dat1)[20] <- "HARVEST_YEAR" # rename year column
names(dat1)[7] <- "FWA_WATERSHED_CODE" # rename watershed code column
str(dat1)
# add a column that identifies the main tributaries - this returns a TRUE value for each 
dat1$major_trib <- grepl("100-190442-244975-[[:digit:]]{6}-0{6}-.+", dat1$FWA_WATERSHED_CODE)
# clip the watershed code
dat1$FWA_WATERSHED_CODE_short <- substr(dat1$FWA_WATERSHED_CODE, 1, 24)
# make a key
watershed_group_key <- dat1 %>% filter(major_trib==TRUE) %>% group_by(GNIS_NAME, FWA_WATERSHED_CODE_short) %>% summarise()
# rename key variable
names(watershed_group_key)[1] <- "watershed_group"
# merge with original data, now each row has a major watershed group value
dat <- merge(dat1, watershed_group_key, all.x=T, by="FWA_WATERSHED_CODE_short")
str(dat) 

# get rolling sum of logging in past 15 years, by major trib over time
expand_by <- expand.grid(seq(from=min(dat$HARVEST_YEAR), to=max(dat$HARVEST_YEAR), by=1), unique(dat$GNIS_NAME)) # make a data frame combining all stream names with all harvest years in data set
names(expand_by) <- c("HARVEST_YEAR", "GNIS_NAME")
unique_dat <- dat %>% group_by(GNIS_NAME, AREA_HA, STREAM_ORD, major_trib, watershed_group) %>% summarise() # make a data frame that just has one row per catchment
expand_by_full <- merge(expand_by, unique_dat, by="GNIS_NAME", all=T) # merge with expanded year table
expand_dat <- merge(dat, expand_by_full, by=c("HARVEST_YEAR", "GNIS_NAME", "AREA_HA", "STREAM_ORD", "major_trib", "watershed_group"), all=T) # merge with full data
# replace areaCut_ha NAs with 0
expand_dat$areaCut_ha[is.na(expand_dat$areaCut_ha)] <- 0
# get rolling 20 year sum of area cut
cs_dat <- expand_dat %>% group_by(GNIS_NAME, major_trib, AREA_HA, STREAM_ORD, watershed_group, HARVEST_YEAR) %>% # weird thing on this line, HARVEST_YEAR has to come last in the order in group_by() argument. 
  summarise(year_areaCut_ha = sum(areaCut_ha)) %>% # get yearly totals
  arrange(HARVEST_YEAR) %>% 
  mutate(cum_sum_cut_20yr_ha =rollapplyr(year_areaCut_ha, 20, sum, align="right",partial=TRUE)
)
# calculate % cumulative sum area logged
cs_dat$percent_cut_20yr <- cs_dat$cum_sum_cut_20yr_ha / cs_dat$AREA_HA


# Summarise percent of catchments cut since 2000
sdat <- dat %>% filter(HARVEST_YEAR > 2000) %>% group_by(GNIS_NAME, STREAM_ORD, AREA_HA, major_trib) %>% summarise(clearcut_area_ha = sum(areaCut_ha, na.rm=TRUE))
# calculate percent cut since 2000
sdat$percent_cut_since_2000 <- sdat$clearcut_area_ha / sdat$AREA_HA
head(sdat)

write.csv(sdat, "./data/percent_clearcut_since_2000_subcatchments.csv", row.names = FALSE)

# summarise area cut by year
ysdat <- dat %>% group_by(HARVEST_YEAR, GNIS_NAME, STREAM_ORD, AREA_HA, major_trib) %>% summarise(clearcut_area_ha = sum(areaCut_ha, na.rm=TRUE))
ysdat$percent_cut <- ysdat$clearcut_area_ha / ysdat$AREA_HA

# Get percent cut of Nicola from 2002 to 2009
sum(ysdat$percent_cut[ysdat$HARVEST_YEAR %in% 2003:2011 & ysdat$GNIS_NAME=="Nicola River"])
ggplot(ysdat[ysdat$GNIS_NAME=="Nicola River", ], aes(y=clearcut_area_ha, x=HARVEST_YEAR)) +
  geom_point() 
ggplot(ysdat[ysdat$GNIS_NAME=="Nicola River", ], aes(y=rollapplyr(percent_cut, 20, sum, align="right",partial=TRUE), x=HARVEST_YEAR)) +
  geom_point() 

# Figures -----


# plot Percent cut since 2000 against catchment area, major tribs only
ggplot(sdat[sdat$major_trib==TRUE & sdat$STREAM_ORD < 8,], aes(y=percent_cut_since_2000, x=AREA_HA)) +
  geom_point() + 
  geom_text(aes(label=GNIS_NAME), hjust=-0.1) +
  xlab("Subcatchment area (ha)") +
  ylab("Percent of subcatchment clearcut since 2000 (%)") +
  theme_classic()

# Plot percent of watershed clearcut against catchment area, size by order
ggplot(sdat[sdat$STREAM_ORD <8,], aes(y=percent_cut_since_2000, x=AREA_HA, size=STREAM_ORD)) +
  geom_point() + 
  theme_classic()

# plot nicola only
ggplot(cs_dat[cs_dat$STREAM_ORD>8, ], aes(y=percent_cut_20yr, x= HARVEST_YEAR)) +
  geom_point()

# plot percent area logged in last 20 years over time. Label last point in each group with creek name
fig_percent_logged_over_time <- ggplot(cs_dat[cs_dat$STREAM_ORD < 8 & cs_dat$STREAM_ORD > 4,], aes(y=percent_cut_20yr, x=HARVEST_YEAR, colour=watershed_group, group=GNIS_NAME)) + 
  geom_line(size=1.2) +
  geom_text(data=cs_dat[cs_dat$HARVEST_YEAR==max(cs_dat$HARVEST_YEAR) & cs_dat$STREAM_ORD < 8 & cs_dat$STREAM_ORD > 4,], aes(label=GNIS_NAME), hjust=-0.1) +
  scale_x_continuous(limits=c(min(cs_dat$HARVEST_YEAR), 2030)) +
  scale_y_continuous(breaks=seq(0,0.6, 0.1)) +
  ylab("Percent of catchment clearcut in previous 20 years") +
  xlab("Year") +
  scale_colour_discrete(guide="none") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour="gray", linetype=2))
fig_percent_logged_over_time 
ggsave("./figures/fig_percent_logged_over_time.png", fig_percent_logged_over_time )


fig_percent_logged_over_time_all <- ggplot(cs_dat[cs_dat$STREAM_ORD < 8,], aes(y=percent_cut_20yr, x=HARVEST_YEAR, colour=watershed_group, group=GNIS_NAME)) + 
  geom_line(size=1.1, alpha=0.5) +
  #geom_text(data=cs_dat[cs_dat$HARVEST_YEAR==max(cs_dat$HARVEST_YEAR) & cs_dat$STREAM_ORD < 8 ,], aes(label=GNIS_NAME), hjust=-0.1) +
  geom_line(data=cs_dat[cs_dat$GNIS_NAME==cs_dat$watershed_group, ], aes(y=percent_cut_20yr, x=HARVEST_YEAR), colour='black')+ 
  #scale_x_continuous(limits=c(min(cs_dat$HARVEST_YEAR), 2030)) +
  #scale_y_continuous(breaks=seq(0,0.9, 0.1)) +
  ylab("Percent of catchment clearcut in previous 20 years") +
  xlab("Year") +
  facet_wrap(~watershed_group) +
  scale_colour_discrete(guide="none") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour="gray", linetype=2))
fig_percent_logged_over_time_all
ggsave("./figures/fig_percent_logged_over_time_all.png", fig_percent_logged_over_time_all )


# Plot percent logged in last 20 years vs. watershed area
fig_percent_logged_by_area <- ggplot(cs_dat[cs_dat$HARVEST_YEAR==max(cs_dat$HARVEST_YEAR) & cs_dat$STREAM_ORD < 8 & cs_dat$STREAM_ORD > 3,], aes(y=percent_cut_20yr, x=AREA_HA, colour=watershed_group)) + 
  geom_point(size=2) +
  geom_text(aes(label=GNIS_NAME), hjust=-0.1) +
  geom_mark_hull(concavity = 5,expand=0,radius=0, aes(fill=watershed_group))+  
  scale_y_continuous(breaks=seq(0,0.6, 0.1)) +
  scale_colour_discrete(guide="none") +  
  scale_fill_discrete(guide="none") +
  xlab("Area of catchment (ha)") +
  ylab("Percent of catchment clearcut in previous 20 years") +
  scale_x_continuous(breaks=seq(0,140000, 20000), limits=c(0, 130000)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour="gray", linetype=2))
fig_percent_logged_by_area
ggsave("./figures/fig_percent_logged_by_area.png", fig_percent_logged_by_area)


fig_percent_logged_by_area_all  <- ggplot(cs_dat[cs_dat$HARVEST_YEAR==max(cs_dat$HARVEST_YEAR) & cs_dat$STREAM_ORD < 8,], aes(y=percent_cut_20yr, x=AREA_HA, colour=watershed_group)) + 
  geom_point(size=2) +
  #geom_text(data = cs_dat[cs_dat$HARVEST_YEAR==max(cs_dat$HARVEST_YEAR) & cs_dat$STREAM_ORD < 8 & cs_dat$major_trib==TRUE,], aes(label=GNIS_NAME), hjust=-0.1) +
  geom_mark_hull(concavity = 5,expand=0,radius=0, aes(fill=watershed_group))+  
  #scale_y_continuous(breaks=seq(0,0.6, 0.1)) +
  scale_colour_discrete(guide="none") +  
  scale_fill_discrete(guide="none") +
  xlab("Area of catchment (ha)") +
  ylab("Percent of catchment clearcut in previous 20 years") +
  facet_wrap(~watershed_group) +
  scale_x_continuous(breaks=seq(0,140000, 20000), limits=c(0, 130000)) +
  theme_classic() + 
  theme(panel.grid.major.y = element_line(colour="gray", linetype=2))
fig_percent_logged_by_area_all
ggsave("./figures/fig_percent_logged_by_area_all.png", fig_percent_logged_by_area_all)



# plot cumulative percent logged over time
ggplot(cs_dat[cs_dat$major_trib==TRUE,], aes(y=percent_cum_sum_cut, x=HARVEST_YEAR, colour=GNIS_NAME, group=GNIS_NAME)) + 
  geom_line() +
  theme_classic()


