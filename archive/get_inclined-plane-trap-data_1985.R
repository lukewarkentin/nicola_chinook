rm(list=ls())

library(rJava)
library(tabulizer)
library(lubridate)
library(ggplot2)
library(tidyr)
library(ggpubr)
library(viridis)

#Read in count data manually ------------
report <- "http://waves-vagues.dfo-mpo.gc.ca/Library/103954.pdf" #
col.brk <- list(c(107,140,168,200,231,260, 290, 315, 340, 375, # col breaks for page 9
                  437, 473, 503, 539, 575, 595, 630, 652, 678),
                c(107,140,170,202,233,262, 292, 317, 342, 377, # col breaks for page 10
                  438, 471, 503, 539, 572, 592, 630, 652, 675),
                c(107,134,174,200,230,260, 290, 315, 340, 375, # col breaks for page 11
                  437, 471, 506, 539, 575, 595, 630, 652, 678))

lst <- extract_tables(report, pages=c(9:11), guess=FALSE, columns=col.brk, encoding="UTF-8") # extract tables
head(lst[[1]], n=5)
head(lst[[2]], n=5)
head(lst[[3]], n=5)

t1 <- lst[[1]][,1:10]
t2 <- lst[[1]][,11:20]
t3 <- lst[[2]][-1,1:10] #remove repeat first row
t4 <- lst[[2]][-1,11:20] #remove repeat first row
t5 <- lst[[3]][,1:10]
t6 <- lst[[3]][,11:20]

ff <-list(t1, t2, t3, t4, t5, t6)
t1
t2
t3
t4
t5
t6

ff2 <- lapply(ff, function(i){
  i <- as.data.frame(i, stringsAsFactors=FALSE) #make into data frame
  names(i) <- c("date", "hrs_fish", "water_temp", "flow_cms",  #rename
                "CH.parr.hatch", "CH.parr.wild", "CH.fry", "CO", "RBT", "OTH")
  i <- i[-c(1,2),] #remove first two rows
  i$date <- ymd(i$date) #make into date format
  i
  i
})

dat <- do.call("rbind", ff2)
dat$flow_cms <- sub(".[[:space:]]", "", dat$flow_cms) #remove random point character
str(dat)
dat <- dat[!is.na(dat$date),] #remove blank rows
dat$CO <-  sub("O|o", "0", dat$CO) # sub out O for 
dat[,-1] <-  apply(dat[,-1], 2, function(x) as.numeric(x)) # change to numeric
str(dat)

dats <- gather(dat, key="spp", value="count", 5:10) #wide to long data
dats$CPUE <- dats$count/dats$hrs_fish

# Read in fish data -------
rpt <- ("http://waves-vagues.dfo-mpo.gc.ca/Library/101883.pdf")
lst2 <- extract_tables(rpt, pages=9, guess=TRUE)
head(lst[[1]])


write.csv(dats, "./data/inclined_plane_trap_data_1985_Lauzier.csv")
