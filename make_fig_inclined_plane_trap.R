library(lubridate)
library(dplyr)


# Read in data already made ----------
dats <- read.csv("./data/inclined_plane_trap_data_1985_Lauzier.csv")
dats$date <- ymd(dats$date)
str(dats)
dats$month <- month(dats$date)
dat_cum_sum <- dats %>% filter(spp=="CH.fry") %>% mutate(cum_prop = cumsum(CPUE)/sum(CPUE)) # get cumulative proportion of CPUE for Chinook fry

# Plot cumulative proportion of fry, fry, wild parr and discharge
png(filename = "./figures/fig_inclined-plane-trap-chinook.png", width=8, height=10, units="in", res=300, pointsize=15)
layout(matrix(c(1,2,3,4), nrow=4, ncol=1,byrow=TRUE))
par(mar=c(0.3, 5, 0.3, 0.3), bty="L")
plot(y=dat_cum_sum$cum_prop, x=dat_cum_sum$date, type="l", xlab="", ylab="Cumulative proportion\nChinook fry CPUE")
text(x=ymd("1985-04-01"), y=0.9, label="a")
abline(h=0.5, lty=3, col="gray")

par(mar=c(0.3, 5, 0.3,0.3), bty="L")
plot(y=dats$CPUE[dats$spp=="CH.fry"], x=dats$date[dats$spp=="CH.fry"],type="l", xlab="",  ylab="Chinook fry CPUE")
text(x=ymd("1985-04-01"), y=25, label="b")

par(mar=c(0.3, 5, 0.3, 0.3))
plot(y=dats$CPUE[dats$spp=="CH.parr.wild"], x=dats$date[dats$spp=="CH.parr.wild"],type="l", xlab="", ylab="Chinook parr (wild) CPUE")
text(x=ymd("1985-04-01"), y=7, label="c")

par(mar=c(4, 5, 0.3, 0.3))
plot(y=dats$flow_cms[dats$spp=="CH.parr.wild"], x=dats$date[dats$spp=="CH.parr.wild"],type="l", xlab="Date", ylab=expression("Nicola River discharge (m"^3*"s"^-1*")"))
text(x=ymd("1985-04-01"), y=150, label="d")

dev.off()