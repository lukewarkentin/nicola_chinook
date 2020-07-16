# Master Code for Nicola recruitment analysis
# Drought and flood effects on Chinook Recruitment in Nicola River
# Luke Warkentin
# Test commit to GitHub

library(tidyhydat)
library(dplyr)
library(ggplot2)
library(rstan)
library(purrr)
library(stringr)
library(lubridate)
library(readxl)
library(RColorBrewer)
library(DMwR)
rstan_options(auto_write=TRUE)

rm(list=ls())

# Data -------------
brood <- read.csv("./data/nicola_brood_table.csv") # read in cohort data table with hatchery data
# manipulate to get recruits
#spawners <- brood %>% group_by(run_year) %>% summarise(total_spawners=sum(total_spawners), wild_spawners=sum(true_wild_spawners))
recruits <- brood %>% group_by(brood_year) %>% summarise(wild_recruits=sum(recruits))

# read in spawner data 
spawners <- read.csv("./data/spawners_1995-2018.csv")

sd <- merge(recruits, spawners, by.x="brood_year", by.y="run_year", all=TRUE)
# write csv to send to Chuck Parken
write.csv(sd, "./data/spawners_recruits_unclipped_hatchery_adjusted_LukeWarkentin.csv", row.names=FALSE)

# Read in calibrated peak count estimates (aerial counts? Check with Chuck Parken) from 1992 to 1994 with CV, and variance and CV for mark-recapture estimates from 1995-2018
spawn_sup <- read_excel("./data/Nicola Calibrated Esc with Revised AFC and unclipped (1975-1994) for Luke.xlsx", trim_ws = TRUE, skip=1)
names(spawn_sup) <- c("spawning_year", "total_spawners", "clipped_spawners", "unclipped_spawners", "CV")
# for years before hatchery influence, make clipped spawners 0 - CHECK WITH CHUCK - hatchery releases started 1984, so 3 year olds would return 1987, which is the first year we have unclipped and clipped counts 
spawn_sup$clipped_spawners[is.na(spawn_sup$clipped_spawners)] <- 0
# For years before hatchery influence, make unclipped spawners = total spawners
spawn_sup$unclipped_spawners[is.na(spawn_sup$unclipped_spawners)] <- spawn_sup$total_spawners[is.na(spawn_sup$unclipped_spawners)]

# Read in unmarked hatchery adults by year to correct spawners for 1992-1994
uh <- read.csv("./data/unmarked_hatchery_returns_by_year.csv")
# merge
spawn_sup1 <- merge(spawn_sup, uh, by.x="spawning_year", by.y="run_year", all.x=TRUE)
# make NA years 0
spawn_sup1$unmarked_hatchery_returns_all_ages[is.na(spawn_sup1$unmarked_hatchery_returns_all_ages)] <- 0
# subtract umarked adults from unclipped to get true wild spawners
spawn_sup1$wild_spawners <- round(spawn_sup1$unclipped_spawners - spawn_sup1$unmarked_hatchery_returns_all_ages, 0)
# if les than 0, make it 0 for wild spawners
spawn_sup1$wild_spawners <- ifelse(spawn_sup1$wild_spawners<0, 0, spawn_sup1$wild_spawners)
# get hatchery spawners
spawn_sup1$hatchery_spawners <- round(spawn_sup1$total_spawners- spawn_sup1$wild_spawners,0)


# read in data with CV for 1992-2018
cv <- read.csv("./data/mark_recap_and_peak_count_estimates_with_variance_and_CV_Nicola.csv")
names(cv) <- c("spawning_year", "spawners", "variance", "CV")

# get total spawners and wild spawners for brood years 1992-1994, which was before mark recapture, into brood table
sd[sd$brood_year %in% 1992:1994, "total_spawners" ] <- round(spawn_sup1[spawn_sup1$spawning_year %in% 1992:1994, "total_spawners"],0)
sd[sd$brood_year %in% 1992:1994, "wild_spawners" ] <- round(spawn_sup1[spawn_sup1$spawning_year %in% 1992:1994, "wild_spawners"],0)
sd[sd$brood_year %in% 1992:1994, "hatchery_spawners" ] <- round(spawn_sup1[spawn_sup1$spawning_year %in% 1992:1994, "hatchery_spawners"],0)

# merge to get variance and CV into brood table
sd <- merge(sd, cv[,c("spawning_year","CV")], by.x="brood_year", by.y="spawning_year", all.x=TRUE)
# add hatchery spawners

# combine old data and new data for graphing
old_to_comb <- spawn_sup1[,c(1,2,5,7,8)]
old_to_comb$wild_recruits <- NA
names(old_to_comb)[grep("spawning_year", names(old_to_comb))] <- "brood_year"

# bind rows of old spawner data with new spawner data
full_spawn <- rbind(old_to_comb, sd[sd$brood_year>=1995,])
# write csv, for graphing full time series
write.csv(full_spawn, "./data/full_spawner_time_series.csv", row.names=FALSE)

# add recruits per spawner variable
sd$recruits_per_spawner <- sd$wild_recruits / sd$total_spawners

# Read in flow data --------
fd <- read.csv("./data/nicola_yearly_flows_all_months.csv") # read in flow data 

# Add flow variables
d <- left_join(sd, fd, by=c("brood_year"="year"), all.x=TRUE)

# Add smolt to age 3 survival
od <- read_excel("./data/Nicola Smolt to Age3 Survival (2019 CWT analysis).xlsx", skip=2)
names(od) <- c("brood_year", "smolt_age3_survival")
# Merge with other data
d <- merge(d, od, by="brood_year", all.x=TRUE)

# Remove rows with incomplete recruits for Stan analysis
d <- d[d$brood_year>=1992 & d$brood_year<=2013, ]

# Centre and standardize all predictor variables
d_unscaled <- d # make unscaled data frame for graphing raw predictor variables
names(d)[8]
d[ ,8:ncol(d)] <- as.numeric(scale(d[ ,8:ncol(d)])) # centre and standardize all predictor variables

# save data to use in model to csv
write.csv(d, "./data/model_data.csv")
write.csv(d_unscaled, "./data/model_data_unscaled.csv")

# save data for DRYAD repository
write.csv(d_unscaled[c(1:5,7,15,22,23, 31,40)], "./data/DRYAD_upload_stock_recruit_model_data_unscaled.csv", row.names = FALSE)

# check
# round(colMeans(d, na.rm=TRUE),1)
# apply(d, 2, sd, na.rm=TRUE)

# Get into model matrix form
# pred_vars <- c("aug_mean_flow", "sep_dec_max_flow", "ice_days", "aug_mean_flow_rear", "smolt_age3_survival")
# mod_matrix <- model.matrix(object = wild_recruits ~ aug_mean_flow + sep_dec_max_flow + ice_days + aug_mean_flow_rear + smolt_age3_survival, data=d)
# colnames(mod_matrix)
# mod_matrix <- mod_matrix[,-grep("(Intercept)", colnames(mod_matrix))] # remove intercept column
# 

# Normalize predictor variables for traffic light plots
# Normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# apply to each column
d_norm<- d_unscaled
d_norm[,9:ncol(d_norm)] <- apply(d_norm[,9:ncol(d_norm)], 2, normalize)
# reverse max flow to get colour right
d_norm$sep_dec_max_flow_rev <- 1 - d_norm$sep_dec_max_flow 
d_norm$ice_days_rev <- 1 - d_norm$ice_days

# Normalize full range of hydrometric data
fd_norm <- fd
# remove 2014 (all NAs for rearing year)
fd_norm <- fd_norm[ !fd_norm$year==2014,]
fd_norm[ , 2:ncol(fd_norm)] <- apply(fd_norm[,2:ncol(fd_norm)], 2, normalize)
# reverse max flow to get colour right
fd_norm$sep_dec_max_flow_rev <- 1 - fd_norm$sep_dec_max_flow 
fd_norm$ice_days_rev <- 1 - fd_norm$ice_days

# # Visual checks

# Check Correlation between covariates
# make vector of predictor variables to check correlation
names(d_unscaled)
pred_plot <- names(d_unscaled)[c(15,22,23,31, 40)]
pred_plot
png("./figures/fig_correlation_predictors.png", height=800, width=1200, pointsize=30)
plot(d_unscaled[ , pred_plot])
dev.off()

# Get correlations 
d_cor <- cor(d_unscaled[ , pred_plot] )
d_cor
write.csv(d_cor, "correlation.csv")

# # Plot recruits as a function of spawners
# ggplot(d, aes(y=wild_recruits, x=total_spawners)) + 
#   geom_point() + 
#   geom_text(aes(label=brood_year))
# 
# # Look at recruits/spawners
# plot(d$wild_recruits/d$total_spawners ~ d$brood_year)
# 
# Examine covariates----------
ggplot(d_unscaled, aes(y=smolt_age3_survival, x=brood_year)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

# ggplot(d, aes(y=mean_flow_aug_rear, x=brood_year)) + 
#   geom_line() + 
#   geom_point() +
#   theme_bw() +
#   scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))
# 
# ggplot(d, aes(y=max_flow_fall, x=brood_year)) + 
#   geom_line() + 
#   geom_point() +
#   theme_bw() +
#   scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))
# 
# ggplot(d, aes(y=prop_wild, x=brood_year)) + 
#   geom_line() + 
#   geom_point() +
#   theme_bw() +
#   scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))
# 
# # Examine centred covariates
# ggplot(d, aes(y=max_flow_fall_c, x=brood_year)) + 
#   geom_line() + 
#   geom_point() +
#   theme_bw() +
#   scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))
# 
# ggplot(d, aes(y=mean_flow_aug_spawn_c, x=brood_year)) + 
#   geom_line() + 
#   geom_point() +
#   theme_bw() +
#   scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))
# 
# ggplot(d, aes(y=ocean_surv_anomaly, x=brood_year)) + 
#   geom_line() + 
#   geom_point() +
#   theme_bw() +
#   scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))
# 
# 
# # Look at recruits ~ spawners with covariates as size
# ggplot(d, aes(y=wild_recruits, x=total_spawners, size=smolt_age3_survival)) + 
#   geom_point()
# 
# ggplot(d, aes(y=wild_recruits, x=total_spawners, size=mean_flow_aug_rearing)) + 
#   geom_point()
# 
# ggplot(d, aes(y=wild_recruits, x=total_spawners, size=max_flow_fall)) + 
#   geom_point()
# 
# ggplot(d, aes(y=wild_recruits, x=total_spawners, size=prop_wild)) + 
#   geom_point()
# 
 # check correlatoin of covariates
# ggplot(d, aes(y=mean_flow_aug_c, x=max_flow_fall_c)) +
#   geom_point() +
#   geom_text(aes(label=brood_year))
# pairs(data=d, ~ mean_flow_aug_c + max_flow_fall_c + ocean_surv_anomaly, lower.panel=NULL) # nothing seems that correlated
 # 

# cov(d[which(apply(d, 1, function(i) all(!is.na(i)))), c("max_flow_fall", "mean_flow_aug_rearing", "max_jan_feb_flow", "smolt_age3_survival")])
# #nothing seems that correlated
# 
# # Plot recruitment over time with percent wild
# plot(d$wild_recruits/d$total_spawners ~ d$brood_year)
# text(y = d$wild_recruits/d$total_spawners, x= d$brood_year, labels=round(d$prop_wild,2 ))
# 
# # spawners
# ggplot(d, aes(y=total_spawners,x=brood_year)) + 
#   geom_line() +
#   geom_line(aes(y=hatch_spawners, x=brood_year), colour="dodgerblue") +
#   geom_line(aes(y=wild_spawners, x=brood_year), colour="gray") +
#   geom_text(aes(y=total_spawners, x=brood_year, label=round(recruits_per_spawner,2))) +
#   theme_bw()
# 
# # Recruits
# #myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), bias=3)
# myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), bias=2)
# 
# ggplot(d, aes(y=wild_recruits,x=brood_year)) + 
#   geom_line() +
#   geom_point(size=5, aes(y=wild_recruits, x=brood_year, colour=mean_flow_aug_rearing)) +
#   geom_text(aes(y=wild_recruits, x=brood_year, label=round(recruits_per_spawner,2))) +
#   scale_colour_gradientn(colours = myPalette(100), name="Mean daily\nflow m3/s") +
#   theme_bw()
# 

# #################################
# # Fit non-linear Stan model #####----------
# #################################
# # think about priors-----------
# # visualize lognormal distribution
# hist(d$wild_recruits)
# mean(d$wild_recruits)
# curve(dlnorm(x, meanlog=0, sdlog=1))
# lookup(dlnorm)
# # For alpha
# hist(d$wild_recruits/d$total_spawners, breaks=seq(0, 14, 1))
# curve(dnorm(x, mean=4, sd =2), add=TRUE)
# 
# # For beta
# curve(dnorm(x, 0.0003, 0.1), from=-0.0001, to=0.001)
# curve(dcauchy(x, location = 0.00018, scale= 0.01))
# 
# # for sigma
# fit_lm <- lm(wild_recruits ~ total_spawners, data=d)
# resid(fit_lm)
# hist(resid(fit_lm))
# curve(dnorm(x, mean=0, sd=1000), col="blue", from=min(resid(fit_lm)), to=max(resid(fit_lm)))
# curve(dlnorm(x, meanlog = 0, sdlog=exp(500)), col="red", from=min(resid(fit_lm)), to=max(resid(fit_lm)))

# Model comparison loop
# l <- rep(list(0:1), 5) # make a list of 5 vectors that are 0,1
# mod_combs <- expand.grid(l) # expand into a table that has all combinations of 0,1 for 5 columns
# mod_matrix[ ,as.numeric(mod_combs[20,])] # check
# 
# for(i in 1:nrow(mod_combs)) {
#   m <- mod_matrix[ ,as.numeric(mod_combs[i,])]
#   print(m)
# }

# Declare data, use centered covariates ----------
#data 
dat <- list(
  N = nrow(d),
  log_RS = log(d$wild_recruits/d$total_spawners),
  Sw = d$wild_spawners, 
  Sh = d$hatchery_spawners,
  S = d$total_spawners,
  ocean_surv = d$smolt_age3_survival,
  aug_mean_flow = d$aug_mean_flow,
  sep_dec_max_flow = d$sep_dec_max_flow,
  aug_mean_flow_rear = d$aug_mean_flow_rear,
  ice_days = d$ice_days
)
# Model 8b---------
# Most parsimonious (from model comparison)
inits_8b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_8b <- c("alpha","beta","b1",
                   "b2", 
                   "b4", 
                   "b5",
                   "tau", "pp_log_RS",  "log_lik")

# Model 8---------
# Two beta model that was most parsimonious = model 8 

inits_8= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         betaH = rnorm(1, 0.0002, 0.0001), betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_8 <- c("alpha","betaW", "betaH","b1",
                  "b2", 
                  "b4", 
                  "b5",
                  "tau", "pp_log_RS",  "log_lik")

 # Fit stan model------------

# fit Model 8b (one beta)
fit_ricker_8b <- stan( file = "ricker_linear_logRS_8b.stan", 
                       data=dat, chains=3, iter=10000, init=inits_8b, 
                       cores=2, pars=pars_track_8b)

# fit Model 8 (two betas, wild and hatchery)
fit_ricker_8 <- stan( file = "ricker_linear_logRS_8.stan", 
                      data=dat, chains=3, iter=10000, init=inits_8, 
                      cores=2, pars=pars_track_8)

# make output into data frame
post <- as.data.frame(fit_ricker_8b)
write.csv(post, "./data/posterior_samples.csv")


# parameters to graph
pars_graph_8b <- c("alpha", 
                "beta",
                "b1", 
                "b2", 
                #"b3", 
                "b4", 
                "b5", 
                "tau"
)
pars_graph_8 <- c("alpha", 
                   "betaH",
                  "betaW",
                   "b1", 
                   "b2", 
                   #"b3", 
                   "b4", 
                   "b5", 
                   "tau"
)

# Plot estimates and CIs
png(filename="./figures/fig_estimates_CI.png", width=700, height=500)
plot(fit_ricker_8b, pars=pars_graph_8b)
dev.off()

# Plot estimates and CIs for covariate effects only
png(filename="./figures/fig_estimates_CI_covariates_only.png", width=300, height=500)
plot(fit_ricker_8b, pars=pars_graph_8b[3:6])
dev.off()

# Get model summary model 8b
mod_sum_8b <- round(summary(fit_ricker_8b,pars= pars_graph_8b, probs=c(0.1,0.9))$summary,6)
mod_sum_8b
write.csv(mod_sum_8b, "estimates.csv")

# Get model summary for model 8
mod_sum_8 <- round(summary(fit_ricker_8,pars= pars_graph_8, probs=c(0.1,0.9))$summary,6)
mod_sum_8 

# Correlation 
# pairs(fit_ricker, pars=pars_graph)

# Traceplots
png(filename="./figures/fig_traceplot.png", width=700, height=700)
traceplot(fit_ricker_8b, pars=pars_graph_8b)
dev.off()

# Supplemental figure - show difference between hatchery and wild spawners ----------
mod_sum_8b
mod_sum_8
png(filename="./figures/fig_ricker-hatchery-wild.png", width=8, height=6, units="in", res=300, pointsize=15)
plot(d$wild_recruits~ d$total_spawners, xlab="Total spawners", ylab="Wild recruits")
# add hatchery curve
curve(mod_sum_8[1,1]* x*exp(-mod_sum_8[2,1]*x), col="red", add=TRUE)
curve(mod_sum_8[1,1]* x*exp(-mod_sum_8[2,4]*x), col="red", lty=2, add=TRUE)
curve(mod_sum_8[1,1]* x*exp(-mod_sum_8[2,5]*x), col="red", lty=2, add=TRUE)

# add wild curve
curve(mod_sum_8[1,1]* x*exp(-mod_sum_8[3,1]*x), col="blue", add=TRUE)
curve(mod_sum_8[1,1]* x*exp(-mod_sum_8[3,4]*x), col="blue", lty=2, add=TRUE)
curve(mod_sum_8[1,1]* x*exp(-mod_sum_8[3,5]*x), col="blue", lty=2, add=TRUE)

# add total spawners curve (one beta model)
curve(mod_sum_8b[1,1]* x*exp(-mod_sum_8b[2,1]*x), col="gray", add=TRUE)
curve(mod_sum_8b[1,1]* x*exp(-mod_sum_8b[2,4]*x), col="gray", lty=2, add=TRUE)
curve(mod_sum_8b[1,1]* x*exp(-mod_sum_8b[2,5]*x), col="gray", lty=2, add=TRUE)


dev.off()

# Just one beta figure
png(filename="./figures/fig_ricker-one-beta.png", width=8, height=6, units="in", res=300, pointsize=15)
plot(d$wild_recruits~ d$total_spawners, xlab="Total spawners", ylab="Wild recruits")

# add curve from pooled model
curve(mod_sum_8b[1,1]* x*exp(-mod_sum_8b[2,1]*x), add=TRUE)
curve(mod_sum_8b[1,1]* x*exp(-mod_sum_8b[2,4]*x), lty=2, add=TRUE)
curve(mod_sum_8b[1,1]* x*exp(-mod_sum_8b[2,5]*x), lty=2, add=TRUE)
dev.off()

# compare carring capacities
cc_w <- log(mod_sum_8[1,1])/mod_sum_8[2,1]
cc_h <- log(mod_sum_8[1,1])/mod_sum_8[3,1]
cc_h / cc_w

# # Slot machine figures - Recruits and spawners with covariates --------------


# #summarise d wide to long format
d1_sum <- d_norm[ ,c("smolt_age3_survival", "aug_mean_flow", "sep_dec_max_flow_rev", "aug_mean_flow_rear", "ice_days_rev", "brood_year", "recruits_per_spawner", "wild_recruits", "prop_wild", "total_spawners")] %>%
  tidyr::gather(., key="variable", value="value", c("smolt_age3_survival", "aug_mean_flow", "sep_dec_max_flow_rev", "ice_days_rev", "aug_mean_flow_rear"), -c("brood_year", "recruits_per_spawner", "wild_recruits", "total_spawners", "prop_wild")) 
str(d1_sum)
# change variable to a factor for graphing
d1_sum$variable <- as.factor(d1_sum$variable)
# reorder levels for graphing, according to order in life cycle
d1_sum$variable <- factor(d1_sum$variable, levels = c("aug_mean_flow", "sep_dec_max_flow_rev", "ice_days_rev", "aug_mean_flow_rear", "smolt_age3_survival"))

#myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral")) # get colour palette
myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "RdYlGn"), bias=1.5) # get colour palette

# make data frame of mean ricker curve predictions
S <- seq(min(d$total_spawners), max(d$total_spawners), by=100)
alpha <- mod_sum[1,1]
beta <- mod_sum[2,1]
R <- alpha* S * exp(-beta*S)
ricker <- data.frame(S,R)

# Plot recruits per spawner over time
fig_recruitment_covariates <-
  ggplot(d1_sum, aes(y=recruits_per_spawner, x=brood_year)) +
  geom_line(colour="black") +
  geom_point(aes(shape=variable, colour=value), size=5, position=position_dodge(width=0.7)) +
  #geom_text(aes(label=round(prop_wild,2))) +
  scale_colour_gradientn(colours = myPalette(100)) +
  scale_shape_manual(values=rep(19,5)) +
  theme_bw()
fig_recruitment_covariates
ggsave("./figures/fig_recruitment_covariates.png", fig_recruitment_covariates, width=12, height=8)

# Plot recruits as a function of spawners (ricker) 
fig_recruits_spawners_covariates <-
  ggplot(d1_sum, aes(y=wild_recruits, x=total_spawners)) +
  geom_point( aes( shape=variable, colour=value), size=4, position=position_dodge(width=1400)) +
  geom_text( aes( label=brood_year), nudge_y=500, colour="black") +
  scale_colour_gradientn(colours = myPalette(100) ) +
  geom_line(data=ricker, aes(y=R, x=S), colour="black") + 
  scale_shape_manual(values=rep(15,5)) + 
  theme_bw()
fig_recruits_spawners_covariates
ggsave("./figures/fig_recruits_spawners_covariates.png", fig_recruits_spawners_covariates, width=12, height=8)

# just plot recruits and spawners and ricker
fig_recruits_spawners <-
  ggplot(d1_sum, aes(y=wild_recruits, x=total_spawners)) +
  geom_point() +
  geom_text( aes( label=brood_year), nudge_y=500, colour="black") +
  geom_line(data=ricker, aes(y=R, x=S), colour="black") + 
  theme_bw()
fig_recruits_spawners
ggsave("./figures/fig_recruits_spawners.png", fig_recruits_spawners, width=12, height=8)


# graph all normalized covariates time series
fig_covariates <- ggplot(d1_sum, aes(y=value, x=brood_year, colour=variable)) +
  geom_line(size=2) +
  #geom_point(size=3) +
  geom_point(aes(y=rep(1, nrow(d1_sum)), x=brood_year, size=round(recruits_per_spawner, 1)), colour="black") +
  scale_colour_manual(values=c("darkred", "goldenrod1",  "turquoise1","salmon", "dodgerblue")) +
  scale_x_continuous(breaks=seq(min(d1_sum$brood_year), max(d1_sum$brood_year), 1)) +
  theme_bw()
fig_covariates
ggsave("./figures/fig_covariates.png", fig_covariates, width=12, height=3)

# Look at full time series of hydrometric covariates
# Plot correlation between all hydrometric variables for all years with complete measurement periods (e.g., complete august, sep-dec, and winter ice period)
plot_vars <- c("aug_mean_flow", "sep_dec_max_flow", "ice_days", "aug_mean_flow_rear")
plot(fd[ , plot_vars])

# plot as a time series of normalized variables
# wide to long format
fd_norm_sum <- fd_norm[ ,c("aug_mean_flow", "sep_dec_max_flow_rev", "aug_mean_flow_rear", "ice_days_rev", "year")] %>%
  tidyr::gather(., key="variable", value="value", c("aug_mean_flow", "sep_dec_max_flow_rev", "ice_days_rev", "aug_mean_flow_rear"), -c("year")) 
fd_long <- fd[ ,c("aug_mean_flow", "sep_dec_max_flow", "aug_mean_flow_rear", "ice_days", "year")] %>%
  tidyr::gather(., key="variable", value="value", c("aug_mean_flow", "sep_dec_max_flow", "ice_days", "aug_mean_flow_rear"), -c("year")) 

cor(fd[ , plot_vars], use="complete.obs")

# Plot time series
fig_hyd_covariates_full <- ggplot(fd_norm_sum[fd_norm_sum$year>=1958,], aes(y=value, x=year, colour=variable)) +
  geom_line(size=2) +
  #geom_point(size=3) +
  geom_point(data=d1_sum, aes(y=rep(1, nrow(d1_sum)), x=brood_year, size=round(recruits_per_spawner, 1)), colour="black") +
  scale_colour_manual(values=c("darkred", "goldenrod1",  "turquoise1","salmon")) +
  scale_x_continuous(breaks=seq(1958, max(fd_norm_sum$year), 1)) +
  theme_bw() + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5))
fig_hyd_covariates_full

fig_raw_hyd_covariates <- ggplot(fd_long[fd_long$year>=1958 & !fd_long$variable=="aug_mean_flow_rear",], aes(y=value, x=year)) +
  geom_line() +
  #geom_point(size=3) +
  #geom_point(data=d1_sum, aes(y=rep(1, nrow(d1_sum)), x=brood_year, size=round(recruits_per_spawner, 1)), colour="black") +
  #scale_colour_manual(values=c("darkred", "turquoise1","salmon")) +
  #scale_x_continuous(breaks=seq(1958, max(fd_long$year), 1)) +
  facet_wrap(~variable, scales="free_y", ncol=1) + 
  theme_bw() + 
  #stat_smooth() +
  theme(axis.text.x=element_text(angle=90, vjust=0.5))
fig_raw_hyd_covariates 
ggsave("./figures/fig_raw_hyd_covariates.png", fig_raw_hyd_covariates, width=10, height=12)


