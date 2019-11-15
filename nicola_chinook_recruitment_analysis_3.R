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
spawners <- brood %>% group_by(run_year) %>% summarise(total_spawners=sum(total_spawners), wild_spawners=sum(true_wild_spawners))
recruits <- brood %>% group_by(brood_year) %>% summarise(wild_recruits=sum(recruits))
sd <- merge(recruits, spawners, by.x="brood_year", by.y="run_year", all=TRUE)
sd$prop_wild <- sd$wild_spawners / sd$total_spawners

# Read in calibrated peak count estimates (aerial counts? Check with Chuck Parken) from 1992 to 1994 with CV, and variance and CV for mark-recapture estimates from 1995-2018
spawn_sup <- read.csv("./data/mark_recap_and_peak_count_estimates_with_variance_and_CV_Nicola.csv", strip.white = TRUE)
names(spawn_sup) <- c("spawning_year", "total_spawners", "variance", "CV")
# get total spawners for brood years 1992-1994, which was before mark recapture, into brood table
sd[is.na(sd$total_spawners) & sd$brood_year %in% 1992:1994, "total_spawners" ] <- spawn_sup[spawn_sup$spawning_year %in% 1992:1994, "total_spawners"]
# merge to get variance and CV into brood table
sd <- merge(sd, spawn_sup[,c("spawning_year","variance", "CV")], by.x="brood_year", by.y="spawning_year", all.x=TRUE)
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
names(d)[9]
d[ ,9:ncol(d)] <- as.numeric(scale(d[ ,9:ncol(d)])) # centre and standardize all predictor variables

# save data to use in model to csv
write.csv(d, "./data/model_data.csv")
write.csv(d_unscaled, "./data/model_data_unscaled.csv")

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
pred_plot <- names(d_unscaled)[c(16,23,24,32, 39, 41)]
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
# dat for autocorrelation linear 
#d <- d[-nrow(d), ] # temp remove 2013 from data for checking jan_feb_max_flow (don't have for brood year 2013)

dat <- list(
  N = nrow(d),
  #log_R = log(d$wild_recruits),
  log_RS = log(d$wild_recruits/d$total_spawners),
  S = d$total_spawners, 
  ocean_surv = d$smolt_age3_survival,
  aug_mean_flow = d$aug_mean_flow,
  sep_dec_max_flow = d$sep_dec_max_flow,
  aug_mean_flow_rear = d$aug_mean_flow_rear,
  ice_days = d$ice_days
)

# inits for autocorrelation
inits= rep(
  list(
    list(#alpha=rnorm(1, mean= 3, sd= 1), #for non-linear
         lnalpha = runif(1, 0,3), # for linear
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         #b6 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2),
         phi = runif(1, -0.99, 0.99),
         log_resid0 = rnorm(1, mean=0, sd = rgamma(1, shape=0.01, scale=0.01) * (1- runif(1, -0.99, 0.99)^2))
    )), 3
)

# inits no autocorrelation
inits2= rep(
  list(
    list(#alpha=rnorm(1, mean= 3, sd= 1), #for non-linear
      lnalpha = runif(1, 0,3), # for linear
      beta = rnorm(1, 0.0002, 0.0001),
      b1 = rnorm(1, mean=0, sd=0.1),
      b2 = rnorm(1, mean=0, sd=0.1),
      b3 = rnorm(1, mean=0, sd=0.1),
      b4 = rnorm(1, mean=0, sd=0.1),
      b5 = rnorm(1, mean=0, sd=0.1),
      #b6 = rnorm(1, mean=0, sd=0.1),
      tau =  runif(1, 0, 2)
    )), 3
)

# fit ricker with autocorrelation
# parameters for model 
pars_track <- c("alpha", "beta","b1",
                "b2", 
                "b3", 
                "b4", 
                "b5", 
                #"b6", 
                "tau", "phi", "log_resid0", "log_resid", "tau_red", "pp_log_RS", "pp_R",
                "log_lik")

# pars to track - no autocorrelation
pars_track2 <- c("alpha", "beta","b1",
                "b2", 
                "b3", 
                "b4", 
                "b5", 
                #"b6", 
                "tau", "pp_log_RS", "pp_R")

# # Fit stan model
# fit_ricker <- stan( file = "ricker_linear_logRS.stan", 
#                       data=dat, chains=3, iter=10000, init=inits, 
#                       #control=list(adapt_delta=0.9),
#                       cores=2, pars=pars_track)

# fit model no autocorrelation
fit_ricker <- stan( file = "ricker_linear_logRS_full.stan", 
                    data=dat, chains=3, iter=10000, init=inits2, 
                    #control=list(adapt_delta=0.9),
                    cores=2, pars=pars_track2)

# make output into data frame
post <- as.data.frame(fit_ricker)
write.csv(post, "./data/posterior_samples.csv" )

# parameters to graph
pars_graph <- c("alpha", "beta","b1", 
                "b2", 
                "b3", 
                "b4", 
                "b5", 
                #"b6", 
                "tau"#, 
                #"phi", "log_resid0"
)

# Plot estimates and CIs
png(filename="./figures/fig_estimates_CI.png", width=700, height=500)
plot(fit_ricker, pars=pars_graph)
dev.off()

# Plot estimates and CIs for covariate effects only
png(filename="./figures/fig_estimates_CI_covariates_only.png", width=300, height=500)
plot(fit_ricker, pars=pars_graph[3:7])
dev.off()

# Get model summary
mod_sum <- round(summary(fit_ricker,pars= pars_graph, probs=c(0.1,0.9))$summary,6)
mod_sum 
write.csv(mod_sum, "estimates.csv")
# Correlation 
# pairs(fit_ricker, pars=pars_graph)

# Traceplots
png(filename="./figures/fig_traceplot.png", width=700, height=700)
traceplot(fit_ricker, pars=pars_graph)
dev.off()


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


