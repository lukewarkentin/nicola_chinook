library(lubridate)
library(dplyr)
library(ggplot2)

wd <- read.csv("./data/weather_data.csv")
wd$date <- ymd(wd$date)
wd$month <- month(wd$date, abbr=TRUE, label=TRUE)
unique(wd$station_name)

stns <- c("MERRITT", "MERRITT STP")
#monthly mean air temperature 
fig_mean_air_temp_month <- wd[wd$station_name %in% stns, ] %>% ggplot(aes(y=mean_temp, x=year)) +
  geom_point(shape=1, colour="gray") +
  geom_hline(aes(yintercept=0), colour="cyan") +
  stat_summary(fun.y="mean", na.rm=TRUE, geom="path", colour="black", size=1.3) +
  facet_wrap(~month, scales="free_y") +
  stat_smooth(method="loess",  colour="red", linetype=2, se=FALSE) +
  theme_bw()
fig_mean_air_temp_month
ggsave("./figures/fig_mean_air_temp_month.png", width=18, height=10)
