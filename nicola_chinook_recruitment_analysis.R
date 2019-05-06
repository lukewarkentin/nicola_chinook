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
rstan_options(auto_write=TRUE)

rm(list=ls())

setwd("D:/22_masters/100.002_Nicola-Chinook-cohorts/9_R/nicola_chinook")

# Data -------------
sd <- read.csv("nicola_spawner_recruits_data.csv") # read in cohort data table with hatchery data
str(sd)

fd <- read.csv("nicola_yearly_flows.csv") # read in flow data 
str(fd)

# remove years where we dodn't have enought data to get recruits, and don't have spawner estimates
sd <- sd[sd$brood_year %in% 1987:2012, ] # get 1987-2012 rows

# Add flow variables
d <- left_join(sd, fd, by=c("brood_year"="year"))

# Centre and standardize covariates
# Centre and standardize estimated ocean survival into anomaly
d$ocean_surv_anomaly <- (d$hatchery_survival - mean(d$hatchery_survival))/sd(d$hatchery_survival)
d$max_flow_fall_c <- (d$max_flow_fall - mean(d$max_flow_fall))/sd(d$max_flow_fall) # standardize the covariates
d$mean_flow_aug_c <- (d$mean_flow_aug_rearing - mean(d$mean_flow_aug_rearing))/sd(d$mean_flow_aug_rearing) # standardize the covariates
#d$max_flow_spring_c <- (d$max_flow_spring - mean(d$max_flow_spring))/sd(d$max_flow_spring) # optional - standardize the covariates

# Visual checks

# Plot recruits as a function of spawners
ggplot(d, aes(y=wild_recruits, x=total_spawners)) + 
  geom_point() + 
  geom_text(aes(label=brood_year))

# Examine covariates----------
# check ocean survival anomaly
ggplot(d, aes(y=ocean_surv_anomaly, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

ggplot(d, aes(y=wild_recruits, x=total_spawners, size=mean_flow_aug_rearing)) + 
  geom_point()

ggplot(d, aes(y=wild_recruits, x=total_spawners, size=max_flow_fall)) + 
  geom_point()

ggplot(d, aes(y=max_flow_fall_c, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

ggplot(d, aes(y=mean_flow_aug_c, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

# check correlatoin of covariates
ggplot(d, aes(y=mean_flow_aug_c, x=max_flow_fall_c)) +
  geom_point() +
  geom_text(aes(label=brood_year))
pairs(data=d, ~ mean_flow_aug_c + max_flow_fall_c + ocean_surv_anomaly) # nothing seems that correlated


#################################
# Fit non-linear Stan model #####----------
#################################
# think about priors-----------
# visualize lognormal distribution
hist(d$wild_recruits)
mean(d$wild_recruits)
curve(dlnorm(x, meanlog=0, sdlog=1))
lookup(dlnorm)
# For alpha
hist(d$wild_recruits/d$total_spawners, breaks=seq(0, 14, 1))
curve(dnorm(x, mean=4, sd =2), add=TRUE)

# For beta
curve(dnorm(x, 0.0003, 0.1), from=-0.0001, to=0.001)
curve(dcauchy(x, location = 0.00018, scale= 0.01))

# for sigma
fit_lm <- lm(wild_recruits ~ total_spawners, data=d)
resid(fit_lm)
hist(resid(fit_lm))
curve(dnorm(x, mean=0, sd=1000), col="blue", from=min(resid(fit_lm)), to=max(resid(fit_lm)))
curve(dlnorm(x, meanlog = 0, sdlog=exp(500)), col="red", from=min(resid(fit_lm)), to=max(resid(fit_lm)))

# Declare data, use centered covariates ----------
str(d)
dat <- list(
  N = nrow(d),
  R = d$wild_recruits,
  S = d$total_spawners,
  OSA = d$ocean_surv_anomaly,
  max_flow_fall = d$max_flow_fall_c,
  mean_flow_aug = d$mean_flow_aug_c 
)

# inits for lognormal
inits=rep(
  list(
    list(alpha=rnorm(1, mean= 3, sd= 1),
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1) ,
         sigma = runif(1, 0.1, 5) # taken from Poisson Consulting https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html
    )), 3
)
inits

# Fit non-linear model without spring flow
fit_ricker_1 <- stan( file = "ricker_nonlinear_lognormal.stan", 
                      data=dat, chains=3, iter=3000, init=inits, 
                      cores=2, pars=c("alpha", "beta", "b1", "b2", "b3", "sigma", "pp_R", "log_lik"))

# make output into data frame
post2 <- as.data.frame(fit_ricker_1)
png(filename="fig_estimates_CI.png", width=300, height=500)
plot(fit_ricker_1, pars=c("alpha", "beta", "b1", "b2", "b3", "sigma"))
dev.off()

# Examine model - lognormal with sigma------------
write.csv(summary(fit_ricker_1,pars= c("alpha", "beta", "b1", "b2", "b3", "sigma"), probs=c(0.1,0.9))$summary, "estimates.csv")
summary(fit_ricker_1,pars= c("alpha", "beta", "b1", "b2", "b3", "sigma"), probs=c(0.1,0.9))$summary
traceplot(fit_ricker_1, pars=c("alpha", "beta", "b1", "b2", "b3", "sigma"))
#pairs(fit_ricker_1, pars=c("alpha", "beta", "b1","b2", "b3", "sigma"))
plot(fit_ricker_1, pars=c("alpha", "beta", "b1", "b1","b2", "b3", "sigma"))

# Plot data with model, using last 100 posterior samples and each 26 lines of observed data -------
png(filename = "fig_R~S_with_model_runs_lognormal.png", width=1000, height=800, pointsize = 25)
par(mar=c(4,4,0,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, xlab="Spawners", ylab="Recruits")
for(i in 1:nrow(d)) {
  for(j in 4400:4500) {
    curve(post2$alpha[j] * x * exp(-post2$beta[j] * x + 
                                     post2$b1[j] * d$ocean_surv_anomaly[i] +
                                     post2$b2[j] * d$max_flow_fall_c[i] + 
                                     post2$b3[j] * d$mean_flow_aug_c[i]), 
          add=TRUE, lwd=2, col=adjustcolor("grey", 0.1))
  }
}
points(d$wild_recruits ~ d$total_spawners)
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) + 
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
dev.off()

# Look at posteriors
rethinking::dens(post2$alpha)
rethinking::dens(post2$beta)
rethinking::dens(post2$b1)
abline(v=0)
rethinking::dens(post2$b2)
abline(v=0)
rethinking::dens(post2$b3)
abline(v=0)
rethinking::dens(post2$sigma)

# Plot triptych plot, min flood, mean flood, max flood
png(filename="fig_flood_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
par(mar=c(4,4,4,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max flood effects on recruitment", xlab="Spawners", ylab="Recruits")
text( d$total_spawners, d$wild_recruits, labels=round(d$max_flow_fall, 0), adj=c(0,0), cex= 0.7)
# Min flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * min(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
# Mean flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# Max flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * max(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
dev.off()

# Plot triptych plot, min, mean, max Aug flow
png(filename="fig_aug_flow_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
par(mar=c(4,4,4,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max August mean flow effects on recruitment", xlab="Spawners", ylab="Recruits")
text( d$total_spawners, d$wild_recruits, labels=round(d$mean_flow_aug_rearing, 0), adj=c(0,0), cex= 0.7)
# Min flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * min(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# Mean flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# Max flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * max(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
dev.off()

# Plot triptych plot, min, mean, max Ocean survival anomaly
png(filename="fig_ocean_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
par(mar=c(4,4,4,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max ocean survival anomaly effects on recruitment", xlab="Spawners", ylab="Recruits")
text( d$total_spawners, d$wild_recruits, labels=round(d$ocean_surv_anomaly, 1), adj=c(0,0), cex= 0.7)
# Min flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * min(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# Mean flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# Max flood
curve(mean(post2$alpha) * x * exp(-mean(post2$beta) * x + 
                                    mean(post2$b1) * max(d$ocean_surv_anomaly) +
                                    mean(post2$b2) * mean(d$max_flow_fall_c) +
                                    mean(post2$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
dev.off()

# plot data with predicted intervals
# get posterior predictions for spawners
ppd <- post2[, grep("pp_R", colnames(post2))]
head(ppd)
mn_ppd <- apply(ppd,2,mean) # get mean of predicted
ci_ppd <- apply(ppd,2,rethinking::HPDI,prob=0.89) # get CI of predicted
plot(d$wild_recruits ~ d$total_spawners , ylim=c(min(ci_ppd), max(c(ci_ppd, d$wild_recruits))))
points(x=d$total_spawners, y=mn_ppd, add=TRUE, col="dodger blue")
segments(x0= d$total_spawners, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1, col="dodger blue")

# Plot time series with predicted intervals
png(filename="fig_predicted_time_series_with_covariates_lognormal.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(y=d$wild_recruits, x=d$brood_year, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Brood year", ylab="Recruits")
lines(y=mn_ppd, x=d$brood_year, col="dodger blue", add=TRUE)
abline(h=0, lty=2, add=TRUE)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd[1,], rev(ci_ppd[2,])), col = adjustcolor('grey', alpha=0.5), border = NA, add=TRUE)
dev.off()

# plot predicted vs observed
png(filename="fig_predicted~observed_with_covariates_lognormal.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(mn_ppd ~ d$wild_recruits, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Observed recruits", ylab="Predicted recruits")
segments(x0= d$wild_recruits, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()

# Look at residuals
hist(d$wild_recruits - mn_ppd) # some right skewed ness
# look at how gamma might help
plot(d$wild_recruits-mn_ppd ~ d$total_spawners) # looks pretty good, bigger residuals at intermediate values of observed recruits
abline(h=0, add=TRUE)
plot(d$wild_recruits-mn_ppd ~ d$wild_recruits) 
abline(h=0, add=TRUE)
