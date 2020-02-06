# Make changing hydrology figures
# hydrographs 
# percent change from historical average

library(tidyhydat)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(EflowStats)
rm(list=ls())