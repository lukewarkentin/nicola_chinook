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

xlims <- c(1900, 2020)
xlims2 <- c(1955, 2020)

# Plot it all!
png("./figures/fig_time_series_context.png", width=8, height=8, units="in", res=300, pointsize=10)
# Layout and formatting
layout(matrix(c(1:8), nrow=4, ncol=2, byrow = FALSE))
par(mar=c(0,4,0,0)+0.1, 
  bty="L")
# weather

# Aug temps
plot(x=aug_temp$year, y=aug_temp$mean, type="b", xlim=xlims, ylab="Mean Aug. air temperature, Merritt BC") #ylim=c(min(aug_temp$mean, na.rm=TRUE), max(aug_temp$max)))
#lines(x=aug_temp$year, y=aug_temp$min, col="gray")
#lines(x=aug_temp$year, y=aug_temp$max, col="gray")

# Jan temps
plot(x=jan_temp$year, y=jan_temp$mean, type="b",  xlim=xlims,ylab="Mean Jan. air temperature, Merritt BC") #ylim=c(min(jan_temp$mean, na.rm=TRUE), max(jan_temp$max)))
abline(h=0, lty=2, col="gray")
#lines(x=jan_temp$year, y=jan_temp$min, col="gray")
#lines(x=jan_temp$year, y=jan_temp$max, col="gray")

# precip
plot(x=rain$year, y=rain$rain, type="l", col="dodgerblue", xlim=xlims, ylab="Precipitation (mm)")
lines(x=snow$year, y=snow$snow, type="l")
legend("topleft", 
       inset=c(0, 0.1),
       legend=c("Rain", "Snow"),
       col=c("dodgerblue", "black"),
       pch="l",
       bty="n" )

par(mar=c(4,4,0,0)+0.1)

# Mean august flow
plot(x=fdma_mean$Year, fdma_mean$Value, type="b" ,  xlim=xlims, ylim=c(0, max(fdma_mean$Value, na.rm=TRUE)), ylab=expression("Mean Aug flow (m"^3*"s"^-1*")"))

par(mar=c(0,4,0,0)+0.1)

# total annual flow
plot(x=fdyt$year, y=fdyt$total_yield, type="b", xlab="Year",  xlim=xlims2, ylab=expression("Total annual yield (m"^3*")"))

# water linences
plot(1,1,  xlim=xlims)

# cutblock area
plot(x=ccs$harvest_year, y=ccs$area_ha, ylab="Clearcut area (ha)", type="l", xlim=xlims2)

par(mar=c(4,4,0,0)+0.1)
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
dev.off()