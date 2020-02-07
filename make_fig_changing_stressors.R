# Make full figure with historical flow, weather, logging, licenese, and spawner data
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyhydat)

# Weather data

wd <- read.csv("./data/weather_data.csv")
wd$date <- ymd(wd$date)
wd$month <- month(wd$date, abbr=TRUE, label=TRUE)
unique(wd$station_name)
stns <- c("MERRITT", "MERRITT STP")
wd <- wd[wd$station_name %in% stns, ]
params <- c("month", "year", "total_rain", "mean_temp", "total_snow")

wd1 <- wd[ , names(wd) %in% params]
wd2 <- wd1[which(apply(wd1, 1, function(x) any(is.na(x)))), ] # remove rows where any of rain, snow or mean_temp are NA

# remove na rows function
rm_na <- function(x)  { x[-which(apply(x, 1, function(i) any(is.na(i)))), ] }

# summarise 
aug_temp <-   wd %>% select(year, month, mean_temp)  %>% rm_na(.) %>% filter( month=="Aug") %>% group_by(year) %>% summarise(min=min(mean_temp, na.rm=TRUE), mean= mean(mean_temp, na.rm=TRUE), max=max(mean_temp, na.rm=TRUE))
jan_temp <-   wd %>% select(year, month, mean_temp) %>% rm_na(.) %>% filter( month=="Jan") %>% group_by(year) %>% summarise(min=min(mean_temp, na.rm=TRUE), mean= mean(mean_temp, na.rm=TRUE), max=max(mean_temp, na.rm=TRUE))
rain <- wd %>% select(year, total_rain) %>% rm_na(.) %>% group_by(year) %>% summarise(rain = sum(total_rain, na.rm=TRUE))
snow <- wd %>% select(year, total_snow) %>% rm_na(.) %>% group_by(year) %>% summarise(snow = sum(total_snow, na.rm=TRUE))

# Clearcut data
cc <- read.csv("./data/Cut_Block_all_BC_Nicola_clip_2019-11-28.csv")
ccs <- cc %>% group_by(HARVEST_YE) %>% summarise(area_ha=sum(AREA_HA))
names(ccs)[1] <- "harvest_year"

# Water licensing 
wl <- read.delim("./data/water-licenses.txt", sep="\t", stringsAsFactors = FALSE) # read in water license data downloaded from http://a100.gov.bc.ca/pub/wtrwhse/water_licences.input, stream name = NICOLA
str(wl)
# Note on units: 
# Unit - The units of measurement for the quantity of water authorized in the licence.
# MD (meters cubic / per day)
# MS (meters cubic / per second)
# MY (meters cubic / per year)
# TF (total flow) - a unit shown against non-consumptive purposes (e.g. land improvement, conservation) for which the total flow of the stream is authorized to pass through the licensed works. No water is diverted from the stream. 
# New column- make all quantities into cubic metres per year
wl$volume_cmy <- ifelse(wl$Unit=="MY", wl$Quantity, 
                        ifelse(wl$Unit=="MD", wl$Quantity* 365, 
                               ifelse(wl$Unit=="MS", wl$Quantity * 60 * 60 * 24 * 365, NA)))
wl$Priority.Date <- ymd(wl$Priority.Date)
wl1 <- wl[-grep("Conservation|Storage", wl$Purpose), ] # remove conservation and dam storages
# get cumulative sum by date
wl2 <- wl1[!is.na(wl1$volume_cmy), ] %>% arrange(Priority.Date) %>%  mutate(cumvolume_cmy = cumsum(volume_cmy))

# Aug mean flow
fdm <- hy_monthly_flows(station_number = "08LG006")
head(fdm)
fdma_mean <- fdm[fdm$Month==8 & fdm$Sum_stat=="MEAN", ]

# total annual yield
fd <- hy_daily_flows(station_number = "08LG006")
fd$year <- year(fd$Date)
fd <- fd[!is.na(fd$Value), ] 
fdy <- fd[fd$year %in% names(which(table(fd$year)>=365)) , ]
head(fdy)
fdyt <- fdy %>% group_by(year) %>% summarise(total_yield = sum(Value)) 

# spawner data
# read in full time series of spawning data
sp <- read.csv("./data/full_spawner_time_series.csv")

#xlims <- c(1910, 2020)
xlims <- c(min(year(wl2$Priority.Date))-2, 2020+2)
xlims2 <- c(1955, 2020)

# Figure 1 : Air temp, precip, water licenses, clearcuts-------
png("./figures/fig_1_change_climate_land_water_use.png", width=4, height=8, units="in", res=300, pointsize=10)
# Layout and formatting
options(scipen=999) # turn off sci. notation
layout(matrix(c(1:5,5), nrow=6, ncol=1, byrow = FALSE))
par(mar=c(0,6,0,0)+0.3, bty="n", xaxs="i", yaxs="r", xaxt="s", las=1, cex=0.9, ann=TRUE)
# weather

# Aug temps
plot(x=aug_temp$year, y=aug_temp$mean, type="b", xlim=xlims, ylab=expression(atop("Mean Aug. air" , paste( "temperature (",degree*C,")"))), col=adjustcolor("black", alpha=0.5), xaxt="n")
axis(side=1, labels=NA, at=seq(1870, 2020, 10))
#ylim=c(min(aug_temp$mean, na.rm=TRUE), max(aug_temp$max)))
#lines(x=aug_temp$year, y=aug_temp$min, col="gray")
#lines(x=aug_temp$year, y=aug_temp$max, col="gray")
lines(loess(mean~year, data=aug_temp)$fitted ~ loess(mean~year, data=aug_temp)$x)
text(x=1880, y=20, label="a")

# Jan temps
plot(x=jan_temp$year, y=jan_temp$mean, type="b",  xlim=xlims, ylab=expression(atop("Mean Jan. air" , paste( "temperature (",degree*C,")"))), col=adjustcolor("black", alpha=0.5), xaxt="n") #ylim=c(min(jan_temp$mean, na.rm=TRUE), max(jan_temp$max)))
axis(side=1, labels=NA, at=seq(1870, 2020, 10))
abline(h=0, lty=2, col="gray")
#lines(x=jan_temp$year, y=jan_temp$min, col="gray")
#lines(x=jan_temp$year, y=jan_temp$max, col="gray")
lines(loess(mean~year, data=jan_temp)$fitted ~ loess(mean~year, data=jan_temp)$x)
text(x=1880, y=-5, label="b")

# precip
plot(x=rain$year, y=rain$rain, type="b", pch=1, col=adjustcolor("black", alpha=0.5), xlim=xlims, ylab="Rain (mm)\nand snow (cm)", ylim=c(min(snow$snow)-10,max(rain$rain) +10 ), xaxt="n")
axis(side=1, labels=NA, at=seq(1870, 2020, 10))
points(x=snow$year, y=snow$snow, type="b", pch= 19, lty=1, adjustcolor("black", alpha=0.5))
lines(loess(rain~year, data=rain)$fitted ~ loess(rain~year, data=rain)$x)
lines(loess(snow~year, data=snow)$fitted ~ loess(snow~year, data=snow)$x)
text(x=c(2000, 2000), y=c(340,10), labels=c("rain", "snow"))
text(x=1880, y=250, label="c")

# legend("topleft", 
#        inset=c(0, 0.1),
#        legend=c("Rain", "Snow"),
#        col=adjustcolor("black", alpha=0.5),
#        pch=c(1,19),
#        bty="n" )

# water linences
plot(wl2$cumvolume_cmy/1000000 ~ year(wl2$Priority.Date), type="b", xlim=xlims, ylab=expression(atop("Water allocations" , (m^3%.%10^6%.%"year"^-1))), xaxt="n")
axis(side=1, labels=NA,at=seq(1870, 2020, 10) )
text(x=1880, y=35, label="d")

# cutblock area
par(mar=c(9,6,0,0)+0.3, bty="n", las=1, xaxt="s", ann=TRUE)
plot(x=ccs$harvest_year, y=cumsum(ccs$area_ha)/7289, ylab="Cumulative percent\nof watershed clearcut", type="b", xlim=xlims, xlab="Year", xaxt="n")
#plot(x=ccs$harvest_year, y=cumsum(ccs$area_ha)/7289, ylab="Cumulative percent\nof watershed clearcut", type="b", xlim=xlims, xlab="Year", xaxt="n", col=ifelse(ccs$harvest_year<2003, "black", "orange"), pch=ifelse(ccs$harvest_year<2003, 1 ,19), cex=0.9)
axis(side=1, at=seq(1870, 2020, 10), labels=seq(1870, 2020, 10), las=2)
text(x=1880, y=20, label="e")

dev.off()



# Fig 2: changes in hydrology-------

# Mean august flow
plot(x=fdma_mean$Year, fdma_mean$Value, type="b" ,  xlim=xlims, ylim=c(0, max(fdma_mean$Value, na.rm=TRUE)), ylab=expression("Mean Aug flow (m"^3*"s"^-1*")"))

par(mar=c(0,4,0,0)+0.1)

# total annual flow
plot(x=fdyt$year, y=fdyt$total_yield, type="b", xlab="Year",  xlim=xlims2, ylab=expression("Total annual yield (m"^3*")"))

# spawners
plot( sp$brood_year,sp$total_spawners ,  type="l", xlab="Year", ylab="Chinook spawners",  xlim=xlims2)
lines(sp$brood_year, sp$wild_spawners, col = "blue", type = "l", add=TRUE)
lines(sp$brood_year, sp$hatchery_spawners, col = "orange", type = "l", add=TRUE)
legend("topleft", 
       inset=c(0, 0.1),
       legend=c("Total", "Wild", "Hatchery"),
       col=c("black", "blue", "orange"),
       pch="l",
       bty="n" )
