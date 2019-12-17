library(ggplot2)

d <- read.csv("./data/VEG_R1_PLY_polygons_Nicola.csv")

round(max(d$PROJ_AGE_1, na.rm=TRUE), -1)

d$age_class <- cut(d$PROJ_AGE_1, 
                   breaks=seq(10,410,10)) 

unique(d$age_class)

length(seq(10,410,10))
length(paste0(seq(0,410,10), "-",seq(0,410,10)+10))

nic_area <- 718379 


ggplot(d[d$BCLCS_LV_2=="T",], aes(y=POLY_AREA, x=age_class)) + 
  geom_col()
