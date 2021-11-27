library(tidyhydat)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(rstan)
library(purrr)
library(stringr)
library(lubridate)
library(readxl)
library(RColorBrewer)
devtools::install_github("cran/DMwR")
library(DMwR)
library(tidyr)
library(EflowStats)
devtools::install_github("rmcelreath/rethinking")
library(rethinking)
# rstan_options(auto_write=TRUE)

rm(list=ls())

# read in data used in model
d <- read.csv("./data_out/model_data.csv")
# read in unscaled data 
d_unscaled <- read.csv("./data_out/model_data_unscaled.csv")
# read in data frame of posterior samples of model parameters
#post <- read.csv("./data/posterior_samples.csv")
# read in data frame of posterior samples of model parameters from model stacking
post <- read.csv("data_out/model_stacking_posterior_samples.csv")

options(scipen = 12) # adjust so estimates are in fixed notation

# Write function to propagate uncertainty in all parameters to get recruits with different inputs 

get_recruits <- function(alpha, beta_t, beta_w, beta_h, b1, b2, b3, b4, b5, 
                         total_spawners, wild_spawners, hatchery_spawners, 
                         smoltsurv, spawnflow, flood, ice,
                         rearflow) {
  logRS = log(alpha) - beta_t * total_spawners - beta_w * wild_spawners - 
          beta_h * hatchery_spawners   + b1 * smoltsurv + b2 * spawnflow + 
          b3 * flood + b4 * ice + b5 * rearflow
  R = exp(logRS) * total_spawners
  estR <- quantile(R, probs=c(0.05, 0.5, 0.95))
  estR
}

# Write function to convert any value into scaled according to actual values
faux_scale <- function(x, scale_by) {
  (x -  mean(scale_by)) / sd(scale_by)
}

# check betas - use not fixed version when including all three beta terms because they are from a model stacked average
plot(density(post$beta))
lines(density(post$beta_fix), col="green")

# get flow data
# UNapproved data downloaded from WSC website Nov 26, 2021 https://wateroffice.ec.gc.ca/download/index_e.html?results_type=real_time
fd <- read.csv("data/nicola_unapproved_flow_2021.csv", skip=10)
names(fd) <- c("date_time", "param_code", "flow_cms")
class(fd$date_time)
fd$date_time <- ymd_hms(fd$date_time)
fd$date <- date(fd$date_time)
fdm <- fd %>% group_by(date) %>% summarise(mean_flow = mean(flow_cms))
fdm$month <- month(fdm$date)
augflowspawn <- mean(fdm$mean_flow[fdm$month==8])

max(d_unscaled$sep_dec_max_flow)
mean(d_unscaled$aug_mean_flow)

# get recruits for average flood
avg_flood <- get_recruits(alpha=post$alpha, beta_t = post$beta, beta_w = post$betaW, beta_h=post$betaH,
             b1 = post$b1, b2 = post$b2, b3 = post$b3, b4=post$b4, b5=post$b5,
             # Data from Chuck Parken 2021-11-26
             # total spawners (rough) = 4,000 with 439 natural and 3,561 hatchery. 
             # the hatchery vs natural is from the tag application sample and 
             # normally we use the carcass sample because it is more representative. 
             # i can't find those data yet, but i have asked Sara for it.
             total_spawners = 4000, wild_spawners = 439, hatchery_spawners = 3561,
             # smolt survival for the most recent completed cohort (2015) is 0.01999491
             smoltsurv= faux_scale(0.01999491,d_unscaled$smolt_age3_survival),
             # from unapproved WSC data, which is 5.2, below the average used to fit data, which was 8.6
             spawnflow = faux_scale(augflowspawn,d_unscaled$aug_mean_flow),
             flood = 0, # average flood
             ice=0, # use mean of data used to fit model, essentially removes effect
             rearflow = 0 # use mean of data used to fit model, essentially removes effect
)  

# get recruits for max observed fall flood from data set used to fit model
max_flood_1992_2013 <- get_recruits(alpha=post$alpha, beta_t = post$beta, beta_w = post$betaW, beta_h=post$betaH,
             b1 = post$b1, b2 = post$b2, b3 = post$b3, b4=post$b4, b5=post$b5,
             # Data from Chuck Parken 2021-11-26
             # total spawners (rough) = 4,000 with 439 natural and 3,561 hatchery. 
             # the hatchery vs natural is from the tag application sample and 
             # normally we use the carcass sample because it is more representative. 
             # i can't find those data yet, but i have asked Sara for it.
             total_spawners = 4000, wild_spawners = 439, hatchery_spawners = 3561,
             # smolt survival for the most recent completed cohort (2015) is 0.01999491
             smoltsurv= faux_scale(0.01999491,d_unscaled$smolt_age3_survival),
             # from unapproved WSC data, which is 5.2, below the average used to fit data, which was 8.6
             spawnflow = faux_scale(augflowspawn,d_unscaled$aug_mean_flow),
             flood = max(d$sep_dec_max_flow), # max flood from model fit data is 208 cms
             ice=0, # use mean of data used to fit model, essentially removes effect
             rearflow = 0 # use mean of data used to fit model, essentially removes effect
             )  

# get recruits for Nov fall flood = 367 cms, max recorded value before station failure # see email from Rich McCleary
est_nov21_flood_367cms <- get_recruits(alpha=post$alpha, beta_t = post$beta, beta_w = post$betaW, beta_h=post$betaH,
                                       b1 = post$b1, b2 = post$b2, b3 = post$b3, b4=post$b4, b5=post$b5,
                                       # Data from Chuck Parken 2021-11-26
                                       # total spawners (rough) = 4,000 with 439 natural and 3,561 hatchery. 
                                       # the hatchery vs natural is from the tag application sample and 
                                       # normally we use the carcass sample because it is more representative. 
                                       # i can't find those data yet, but i have asked Sara for it.
                                       total_spawners = 4000, wild_spawners = 439, hatchery_spawners = 3561,
                                       # smolt survival for the most recent completed cohort (2015) is 0.01999491
                                       smoltsurv= faux_scale(0.01999491,d_unscaled$smolt_age3_survival),
                                       # from unapproved WSC data, which is 5.2, below the average used to fit data, which was 8.6
                                       spawnflow = faux_scale(augflowspawn,d_unscaled$aug_mean_flow),
                                       flood = faux_scale(367, d_unscaled$sep_dec_max_flow), # estimated max flow Nov 15
                                       ice=0, # use mean of data used to fit model, essentially removes effect
                                       rearflow = 0 # use mean of data used to fit model, essentially removes effect
)  
  
# get recruits for Nov fall flood = 762 cms # see email from Rich McCleary
est_nov21_flood_762cms <- get_recruits(alpha=post$alpha, beta_t = post$beta, beta_w = post$betaW, beta_h=post$betaH,
             b1 = post$b1, b2 = post$b2, b3 = post$b3, b4=post$b4, b5=post$b5,
             # Data from Chuck Parken 2021-11-26
             # total spawners (rough) = 4,000 with 439 natural and 3,561 hatchery. 
             # the hatchery vs natural is from the tag application sample and 
             # normally we use the carcass sample because it is more representative. 
             # i can't find those data yet, but i have asked Sara for it.
             total_spawners = 4000, wild_spawners = 439, hatchery_spawners = 3561,
             # smolt survival for the most recent completed cohort (2015) is 0.01999491
             smoltsurv= faux_scale(0.01999491,d_unscaled$smolt_age3_survival),
             # from unapproved WSC data, which is 5.2, below the average used to fit data, which was 8.6
             spawnflow = faux_scale(augflowspawn,d_unscaled$aug_mean_flow),
             flood = faux_scale(762, d_unscaled$sep_dec_max_flow), # estimated max flow Nov 15
             ice=0, # use mean of data used to fit model, essentially removes effect
             rearflow = 0 # use mean of data used to fit model, essentially removes effect
)  

comp <- bind_rows(avg_flood, max_flood_1992_2013, est_nov21_flood_367cms, est_nov21_flood_762cms)
comp$scenario <- c("avg_flood", "max_flood_1992_2013", "est_nov21_flood_367cms","est_nov21_flood_762cms")