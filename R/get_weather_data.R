# Get historical weather data

library(weathercan)
library(tidyverse)

#Weather data ------
#Get weather data 
# Stations to dowload
stns_dl <- c(1020, # Merritt
             1022, # Merritt STP
             1015, # Mamit Lake 
             989,  # Brookmere
             1028, # Nicola Lake
             1029, # Nicola Lake
             1030, # Nicola Lake W End
             992 # Douglas Lake
)

#wd$year <- as.numeric(wd$year)
wdd <- weather_dl(stns_dl, interval="day") # get daily data
str(wdd)

write.csv(wdd, "./data_out/weather_data.csv", row.names = FALSE) # write weather data to csv
