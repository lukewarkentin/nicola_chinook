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
rstan_options(auto_write=TRUE)

rm(list=ls())

# Data -------------
brood <- read.csv("./data/nicola_brood_table.csv") # read in cohort data table with hatchery data
str(brood)
# manipulate to get recruits
spawners <- brood %>% group_by(run_year) %>% summarise(total_spawners=sum(total_spawners), wild_spawners=sum(Wild_Spawners))
recruits <- brood %>% group_by(brood_year) %>% summarise(wild_recruits=sum(recruits))
sd <- merge(recruits, spawners, by.x="brood_year", by.y="run_year", all=TRUE)
sd$prop_wild <- sd$wild_spawners / sd$total_spawners
sd$hatch_spawners <- sd$total_spawners - sd$wild_spawners

# Read in flow data --------
fd <- read.csv("./data/nicola_yearly_flows.csv") # read in flow data 
str(fd)

# Add flow variables
d <- left_join(sd, fd, by=c("brood_year"="year"))

# Add smolt to age 3 survival
od <- read_excel("./data/Nicola Smolt to Age3 Survival (2019 CWT analysis).xlsx", skip=2)
str(od) 
names(od) <- c("brood_year", "smolt_age3_survival")
# Merge with other data
d <- merge(d, od, by="brood_year", all.x=TRUE)

# Remove rows with incomplete recruits
d <- d[d$brood_year>=1995 & d$brood_year<=2013, ]

# add recruits per spawner variable
d$recruits_per_spawner <- d$wild_recruits / d$total_spawners

# Centre and standardize covariates
# Centre and standardize estimated ocean survival into anomaly
d$ocean_surv_anomaly <- (d$smolt_age3_survival - mean(d$smolt_age3_survival))/sd(d$smolt_age3_survival)
d$max_flow_fall_c <- (d$max_flow_fall - mean(d$max_flow_fall))/sd(d$max_flow_fall) # standardize the covariates
d$mean_flow_aug_c <- (d$mean_flow_aug_rearing - mean(d$mean_flow_aug_rearing))/sd(d$mean_flow_aug_rearing) # standardize the covariates
# Extra flow variables
d$mean_sep_oct_flow_c <- (d$mean_sep_oct_flow - mean(d$mean_sep_oct_flow))/sd(d$mean_sep_oct_flow) # standardize the covariates
d$mean_jul_flow_c <- (d$mean_jul_flow - mean(d$mean_jul_flow))/sd(d$mean_jul_flow) # standardize the covariates
d$max_jan_feb_flow_c <- (d$max_jan_feb_flow - mean(d$max_jan_feb_flow))/sd(d$max_jan_feb_flow) # standardize the covariates
# Proportion wild
d$prop_wild_c <- (d$prop_wild - mean(d$prop_wild))/sd(d$prop_wild)
# Visual checks

# Plot recruits as a function of spawners
ggplot(d, aes(y=wild_recruits, x=total_spawners)) + 
  geom_point() + 
  geom_text(aes(label=brood_year))

# Look at recruits/spawners
plot(d$wild_recruits/d$total_spawners ~ d$brood_year)

# Examine covariates----------
ggplot(d, aes(y=smolt_age3_survival, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

ggplot(d, aes(y=mean_flow_aug_rearing, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

ggplot(d, aes(y=max_flow_fall, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

ggplot(d, aes(y=prop_wild, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))

# Examine centred covariates
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

ggplot(d, aes(y=ocean_surv_anomaly, x=brood_year)) + 
  geom_line() + 
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks=seq(min(d$brood_year), max(d$brood_year), 1))


# Look at recruits ~ spawners with covariates as size
ggplot(d, aes(y=wild_recruits, x=total_spawners, size=smolt_age3_survival)) + 
  geom_point()

ggplot(d, aes(y=wild_recruits, x=total_spawners, size=mean_flow_aug_rearing)) + 
  geom_point()

ggplot(d, aes(y=wild_recruits, x=total_spawners, size=max_flow_fall)) + 
  geom_point()

ggplot(d, aes(y=wild_recruits, x=total_spawners, size=prop_wild)) + 
  geom_point()

# check correlatoin of covariates
ggplot(d, aes(y=mean_flow_aug_c, x=max_flow_fall_c)) +
  geom_point() +
  geom_text(aes(label=brood_year))
pairs(data=d, ~ mean_flow_aug_c + max_flow_fall_c + ocean_surv_anomaly, lower.panel=NULL) # nothing seems that correlated
pairs(data=d, ~ mean_flow_aug_c + max_flow_fall_c + mean_jul_flow_c + mean_sep_oct_flow_c + max_jan_feb_flow_c + ocean_surv_anomaly) # 
plot(d[, c("max_flow_fall", "mean_flow_aug_rearing", "max_jan_feb_flow", "smolt_age3_survival", "prop_wild")])
# Get correlations 
cor(d[which(apply(d, 1, function(i) all(!is.na(i)))), c("max_flow_fall", "mean_flow_aug_rearing", "max_jan_feb_flow", "smolt_age3_survival")])
cov(d[which(apply(d, 1, function(i) all(!is.na(i)))), c("max_flow_fall", "mean_flow_aug_rearing", "max_jan_feb_flow", "smolt_age3_survival")])
#nothing seems that correlated

# Plot recruitment over time with percent wild
plot(d$wild_recruits/d$total_spawners ~ d$brood_year)
text(y = d$wild_recruits/d$total_spawners, x= d$brood_year, labels=round(d$prop_wild,2 ))

# spawners
ggplot(d, aes(y=total_spawners,x=brood_year)) + 
  geom_line() +
  geom_line(aes(y=hatch_spawners, x=brood_year), colour="dodgerblue") +
  geom_line(aes(y=wild_spawners, x=brood_year), colour="gray") +
  geom_text(aes(y=total_spawners, x=brood_year, label=round(recruits_per_spawner,2))) +
  theme_bw()

# Recruits
#myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), bias=3)
myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), bias=2)

ggplot(d, aes(y=wild_recruits,x=brood_year)) + 
  geom_line() +
  geom_point(size=5, aes(y=wild_recruits, x=brood_year, colour=mean_flow_aug_rearing)) +
  geom_text(aes(y=wild_recruits, x=brood_year, label=round(recruits_per_spawner,2))) +
  scale_colour_gradientn(colours = myPalette(100), name="Mean daily\nflow m3/s") +
  theme_bw()

# Slot machine figures - Recruits and spawners with covariates --------------
d$max_flow_fall_c_rev <- d$max_flow_fall_c * -1 #reverse sign for plots = large floods should be negative (red) 

#summarise d wide to long format
d_sum <- tidyr::gather(d[,c("ocean_surv_anomaly", "mean_flow_aug_c", "max_flow_fall_c_rev", "brood_year", "recruits_per_spawner", "wild_recruits", "total_spawners", "prop_wild")], key="variable", value="value", c("ocean_surv_anomaly", "mean_flow_aug_c", "max_flow_fall_c_rev"), -c("brood_year", "recruits_per_spawner", "wild_recruits", "total_spawners", "prop_wild")) 

myPalette <- colorRampPalette(RColorBrewer::brewer.pal(11, "Spectral"), bias=1.5)

fig_recruitment_covariates <-  
  ggplot(d_sum, aes(y=recruits_per_spawner, x=brood_year)) + 
  geom_line(colour="black") +
  geom_point(aes(shape=variable, colour=value), size=5, position=position_dodge(width=0.6)) +
  geom_text(aes(label=round(prop_wild,2))) +
  scale_colour_gradientn(colours = myPalette(100)) +
  theme_bw()
fig_recruitment_covariates
ggsave("./figures/fig_recruitment_covariates.png", fig_recruitment_covariates, width=12, height=8)

fig_recruits_spawners_covariates <-  
  ggplot(d_sum, aes(y=wild_recruits, x=total_spawners)) + 
  geom_point( aes( shape=variable, colour=value), size=6, position=position_dodge(width=800)) +
  geom_text( aes( label=brood_year), colour="black") +
  scale_colour_gradientn(colours = myPalette(100)) +
  theme_bw()
fig_recruits_spawners_covariates
ggsave("./figures/fig_recruits_spawners_covariates.png", fig_recruits_spawners_covariates, width=12, height=8)

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

# data for extra flow variables
# dat2 <- list(
#   N = nrow(d),
#   R = d$wild_recruits,
#   S = d$total_spawners,
#   OSA = d$ocean_surv_anomaly,
#   max_flow_fall = d$max_flow_fall_c,
#   mean_flow_aug = d$mean_flow_aug_c,
#   mean_jul_flow = d$mean_jul_flow_c,
#   mean_sep_oct_flow = d$mean_sep_oct_flow_c,
#   max_jan_feb_flow = d$max_jan_feb_flow_c
# )

# dat for prop wild
dat3 <- list(
  N = nrow(d),
  R = d$wild_recruits,
  SW = d$wild_spawners,
  SH = d$hatch_spawners,
  OSA = d$ocean_surv_anomaly,
  max_flow_fall = d$max_flow_fall_c,
  mean_flow_aug = d$mean_flow_aug_c
)

# dat for autocorrelation linear 
dat4 <- list(
  N = nrow(d),
  log_R = log(d$wild_recruits),
  SW = d$wild_spawners,
  SH = d$hatch_spawners,
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
         b3 = rnorm(1, mean=0, sd=0.1),
         sigma = runif(1, 0.1, 5) # taken from Poisson Consulting https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html
    )), 3
)
inits

# inits for lognormal with extra flow variables
# inits2=rep(
#   list(
#     list(alpha=rnorm(1, mean= 3, sd= 1),
#          beta = rnorm(1, 0.0002, 0.0001),
#          b1 = rnorm(1, mean=0, sd=0.1),
#          b2 = rnorm(1, mean=0, sd=0.1),
#          b3 = rnorm(1, mean=0, sd=0.1),
#          b4 = rnorm(1, mean=0, sd=0.1),
#          b5 = rnorm(1, mean=0, sd=0.1),
#          b6 = rnorm(1, mean=0, sd=0.1),
#          sigma = runif(1, 0.1, 5) # taken from Poisson Consulting https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html
#     )), 3
# )
# inits2

# inits for prop_wild model
inits3=rep(
  list(
    list(alpha=rnorm(1, mean= 3, sd= 1),
         betaW = rnorm(1, 0.0002, 0.0001),
         betaH = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         sigma = runif(1, 0.1, 5) # taken from Poisson Consulting https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html
    )), 3
)

# inits for autocorrelation
inits4= rep(
  list(
    list(#alpha=rnorm(1, mean= 3, sd= 1), #for non-linear
         lnalpha = runif(1, 0,3), # for linear
         betaW = rnorm(1, 0.0002, 0.0001),
         betaH = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2),
         phi = runif(1, -0.99, 0.99),
         log_resid0 = rnorm(1, mean=0, sd = rgamma(1, shape=0.01, scale=0.01) * (1- runif(1, -0.99, 0.99)^2))
    )), 3
)

# Fit non-linear model 
fit_ricker_1 <- stan( file = "ricker_nonlinear_lognormal.stan", 
                      data=dat, chains=3, iter=3000, init=inits, 
                      cores=2, pars=c("alpha", "beta", "b1", "b2", "b3", "sigma", "pp_R", "log_lik"))

# Fit non-linear model with extra flow variables
fit_ricker_2 <- stan( file = "ricker_nonlinear_lognormal_extra_flow_vars.stan", 
                      data=dat2, chains=3, iter=3000, init=inits2, 
                      cores=2, pars=c("alpha", "beta", "b1", "b2", "b3", "b4", "b5", "b6", "sigma", "pp_R", "log_lik"))

# Fit ricker with separate beta terms for hatchery and wild spawners
fit_ricker_3 <- stan( file = "ricker_nonlinear_lognormal_1.stan", 
                      data=dat3, chains=3, iter=3000, init=inits3, 
                      cores=2, pars=c("alpha", "betaW", "betaH","b1", "b2", "b3", "sigma", "pp_R", "log_lik"))

# fit ricker with autocorrelation
fit_ricker_4 <- stan( file = "ricker_linear_AR.stan", 
                      data=dat4, chains=3, iter=3000, init=inits4, 
                      cores=2, pars=c("alpha", "betaW", "betaH","b1", "b2", "b3", "tau", "phi", "log_resid0", "log_resid", "tau_red", "pp_R", "log_lik"))

# make output into data frame
post <- as.data.frame(fit_ricker_1)
png(filename="./figures/fig_estimates_CI.png", width=300, height=500)
plot(fit_ricker_1, pars=c("alpha", "beta", "b1", "b2", "b3", "sigma"))
dev.off()

# make output into data frame extra flow vars
post2 <- as.data.frame(fit_ricker_2)
png(filename="./figures/fig_estimates_CI_2.png", width=300, height=500)
plot(fit_ricker_2, pars=c("alpha", "beta", "b1", "b2", "b3","b4", "b5","b6",  "sigma"))
dev.off()

# make output into data frame beta for wild and hatchery
post3 <- as.data.frame(fit_ricker_3)
png(filename="./figures/fig_estimates_CI_3.png", width=300, height=500)
plot(fit_ricker_3, pars=c( "alpha", "betaW", "betaH", "b1", "b2", "b3", "sigma"))
dev.off()

# make output into data frame fit 4
post4 <- as.data.frame(fit_ricker_4)
png(filename="./figures/fig_estimates_CI_4.png", width=300, height=500)
plot(fit_ricker_4, pars=c( "alpha", "betaW", "betaH","b1", "b2", "b3", "tau", "phi"))
dev.off()

# Examine model - lognormal with sigma------------
write.csv(summary(fit_ricker_1,pars= c("alpha", "beta", "b1", "b2", "b3", "sigma"), probs=c(0.1,0.9))$summary, "estimates.csv")

# Get model summary
summary(fit_ricker_1,pars= c("alpha", "beta", "b1", "b2", "b3", "sigma"), probs=c(0.1,0.9))$summary
round(summary(fit_ricker_3,pars= c("alpha", "betaW","betaH", "b1", "b2", "b3", "sigma"), probs=c(0.1,0.9))$summary,6)

#pairs(fit_ricker_1, pars=c("alpha", "beta", "b1","b2", "b3", "sigma"))
plot(fit_ricker_1, pars=c("alpha", "beta", "b1", "b1","b2", "b3", "sigma"))

# Traceplots
traceplot(fit_ricker_1, pars=c("alpha", "beta", "b1", "b2", "b3", "sigma"))
traceplot(fit_ricker_3, pars=c("alpha", "betaW", "betaH", "b1", "b2", "b3", "sigma"))
traceplot(fit_ricker_4, pars=c("alpha", "betaW", "betaH","b1", "b2", "b3", "tau", "phi", "log_resid0"))

# Plot data with model, using last 100 posterior samples and each 26 lines of observed data -------
png(filename = "./figures/fig_R~S_with_model_runs_lognormal.png", width=1000, height=800, pointsize = 25)
par(mar=c(4,4,0,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, xlab="Spawners", ylab="Recruits")
for(i in 1:nrow(d)) {
  for(j in 4400:4500) {
    curve(post$alpha[j] * x * exp(-post$beta[j] * x + 
                                     post$b1[j] * d$ocean_surv_anomaly[i] +
                                     post$b2[j] * d$max_flow_fall_c[i] + 
                                     post$b3[j] * d$mean_flow_aug_c[i]), 
          add=TRUE, lwd=2, col=adjustcolor("grey", 0.1))
  }
}
points(d$wild_recruits ~ d$total_spawners)
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) + 
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
dev.off()

# Look at posteriors
rethinking::dens(post$alpha)
rethinking::dens(post$beta)
rethinking::dens(post$b1)
abline(v=0)
rethinking::dens(post$b2)
abline(v=0)
rethinking::dens(post$b3)
abline(v=0)
# rethinking::dens(post2$b4)
# abline(v=0)
# rethinking::dens(post2$b5)
# abline(v=0)
# rethinking::dens(post2$b6)
# abline(v=0)
rethinking::dens(post$sigma)

# Plot triptych plot, min flood, mean flood, max flood
png(filename="./figures/fig_flood_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
par(mar=c(4,4,4,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max flood effects on recruitment", xlab="Spawners", ylab="Recruits")
text( d$total_spawners, d$wild_recruits, labels=round(d$max_flow_fall, 0), adj=c(0,0), cex= 0.7)
# Min flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * min(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
# Mean flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# Max flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * max(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
dev.off()

# Plot triptych plot, min, mean, max Aug flow
png(filename="./figures/fig_aug_flow_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
par(mar=c(4,4,4,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max August mean flow effects on recruitment", xlab="Spawners", ylab="Recruits")
text( d$total_spawners, d$wild_recruits, labels=round(d$mean_flow_aug_rearing, 0), adj=c(0,0), cex= 0.7)
# Min flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * min(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# Mean flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# Max flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * max(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
dev.off()

# Plot triptych plot, min, mean, max Ocean survival anomaly
png(filename="./figures/fig_ocean_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
par(mar=c(4,4,4,0) +0.1)
plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max ocean survival anomaly effects on recruitment", xlab="Spawners", ylab="Recruits")
text( d$total_spawners, d$wild_recruits, labels=round(d$ocean_surv_anomaly, 1), adj=c(0,0), cex= 0.7)
# Min flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * min(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# Mean flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * mean(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# Max flood
curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
                                    mean(post$b1) * max(d$ocean_surv_anomaly) +
                                    mean(post$b2) * mean(d$max_flow_fall_c) +
                                    mean(post$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
dev.off()

# plot data with predicted intervals
# get posterior predictions for spawners
ppd <- post4[, grep("pp_R", colnames(post4))]
# for extra flow vars
# ppd <- post2[, grep("pp_R", colnames(post2))]
head(ppd)
mn_ppd <- apply(ppd,2,mean) # get mean of predicted
median_ppd <- apply(ppd,2,median) # get mean of predicted

ci_ppd <- apply(ppd,2,rethinking::HPDI,prob=0.9) # get CI of predicted
ci_ppd50 <- apply(ppd,2,rethinking::HPDI,prob=0.5) # get CI of predicted
ci_ppd10 <- apply(ppd,2,rethinking::HPDI,prob=0.1) # get CI of predicted

plot(d$wild_recruits ~ d$total_spawners , ylim=c(min(ci_ppd), max(c(ci_ppd, d$wild_recruits))))
points(x=d$total_spawners, y=mn_ppd, add=TRUE, col="dodger blue")
segments(x0= d$total_spawners, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1, col="dodger blue")

# Plot time series with predicted intervals
png(filename="./figures/fig_predicted_time_series_with_covariates_lognormal.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(y=d$wild_recruits, x=d$brood_year, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Brood year", ylab="Recruits")
lines(y=mn_ppd, x=d$brood_year, col="dodger blue", add=TRUE)
lines(y=median_ppd, x=d$brood_year, col="firebrick", add=TRUE)
abline(h=0, lty=2, add=TRUE)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd[1,], rev(ci_ppd[2,])), col = adjustcolor('grey', alpha=0.5), border = NA, add=TRUE)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd50[1,], rev(ci_ppd50[2,])), col = adjustcolor('grey', alpha=0.5), border = NA, add=TRUE)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd10[1,], rev(ci_ppd10[2,])), col = adjustcolor('grey', alpha=0.5), border = NA, add=TRUE)

dev.off()

# plot predicted vs observed
png(filename="./figures/fig_predicted~observed_with_covariates_lognormal.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(mn_ppd ~ d$wild_recruits, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Observed recruits", ylab="Predicted recruits")
segments(x0= d$wild_recruits, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()


# Look at residuals
# histograms
png("./figures/fig_resid_hist.png",  width=1200, height=800, pointsize = 30)
hist(d$wild_recruits - mn_ppd) # some right skewed ness
dev.off()

# Residuals ~ observed
png("./figures/fig_resid~obs.png",  width=1200, height=800, pointsize = 30)
plot(d$wild_recruits - mn_ppd ~ d$wild_recruits) 
dev.off()

# Residuals ~ predicted
png("./figures/fig_resid_pred.png",  width=1200, height=800, pointsize = 30)
plot(d$wild_recruits - mn_ppd ~ mn_ppd) 
dev.off()

# Residuals ~ year
png("./figures/fig_resid_year.png",  width=1200, height=800, pointsize = 30)
plot(d$wild_recruits - mn_ppd ~ d$brood_year) 
dev.off()


# look at how gamma might help
plot(d$wild_recruits-mn_ppd ~ d$total_spawners) # looks pretty good, bigger residuals at intermediate values of observed recruits
abline(h=0, add=TRUE)
plot(d$wild_recruits-mn_ppd ~ d$wild_recruits) 
abline(h=0, add=TRUE)

rethinking::compare(fit_ricker_1, fit_ricker_3)
