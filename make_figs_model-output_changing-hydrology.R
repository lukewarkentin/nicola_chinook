# This code makes the figures based on the posterior distributions from the model, including predictions. 
# It also makes the changing hydrology figures
# It also calculates various statistics for the text of the manuscript.

library(tidyhydat)
library(dplyr)
library(ggplot2)
library(rstan)
library(purrr)
library(stringr)
library(lubridate)
library(readxl)
library(RColorBrewer)
#devtools::install_github("cran/DMwR")
library(DMwR)
library(tidyr)
library(EflowStats)
#devtools::install_github("rmcelreath/rethinking")
library(rethinking)
rstan_options(auto_write=TRUE)

rm(list=ls())

# read in data used in model
d <- read.csv("./data/model_data.csv")
# read in unscaled data 
d_unscaled <- read.csv("./data/model_data_unscaled.csv")
# read in data frame of posterior samples of model parameters
#post <- read.csv("./data/posterior_samples.csv")
# read in data frame of posterior samples of model parameters from model stacking
post <- read.csv("data_out/model_stacking_posterior_samples.csv")

plot(post$alpha~post$beta_fix)

head(post)

# get posterior predictions for log(recruits/spawners)
# compare stacked log(pred_r/s) and recalculated log(pred_r/s) based on stacked posteriors
pp_log_RS_stacked <- post[, grep("pp_log_RS", colnames(post))] # I'm not sure if this works for stacked posteriors
mn_pp_log_RS_stacked <- apply(pp_log_RS_stacked,2,mean) # get mean of predicted
ci_pp_log_RS_stacked <- apply(pp_log_RS_stacked,2,rethinking::PI, prob=0.9) # get CI of predicted


# function to get predicted log(recruits/spawner) for each obseration
get_pred_log_RS <- function(total_spawners, wild_spawners, hatchery_spawners,
                         smolt_age3_surv, aug_flow_spawn, fall_flood, ice_days, aug_flow_rear ) {
  pred_rec <- log(post$alpha) -post$beta * total_spawners - post$betaH* hatchery_spawners - post$betaW * wild_spawners + post$b1 * smolt_age3_surv + post$b2 * aug_flow_spawn + post$b3 * fall_flood + post$b4 * ice_days + post$b5* aug_flow_rear
}
# get repredictions based on full stacked parameter posteriors
pp_log_RS_repred <- pmap_dfc(list(d$total_spawners, d$wild_spawners, d$hatchery_spawners, 
                     d$smolt_age3_survival, d$aug_mean_flow, d$sep_dec_max_flow, 
                     d$ice_days, d$aug_mean_flow_rear), get_pred_log_RS)

# Get summary of log_RS repredicted
mn_pp_log_RS_repred <- apply(pp_log_RS_repred,2,mean) # get mean of predicted
#median_pp_log_RS <- apply(pp_log_RS,2,median) # get mean of predicted
ci_pp_log_RS_repred <- apply(pp_log_RS_repred,2,rethinking::PI,prob=0.9) # get CI of predicted
#ci_pp_log_RS_50 <- apply(pp_log_RS,2,rethinking::PI,prob=0.5) # get CI of predicted
#ci_pp_log_RS_10 <- apply(pp_log_RS,2,rethinking::PI,prob=0.1) # get CI of predicted

# Compare stacked and repredicted log(R/S)
par(mar=c(4,4,0,0) +0.1)
plot(x=mn_pp_log_RS_repred, y=mn_pp_log_RS_stacked, xlim=c(min(ci_pp_log_RS_repred), max(ci_pp_log_RS_repred)), ylim=c(min(ci_pp_log_RS_stacked), max(ci_pp_log_RS_stacked)), xlab="repredicted log(R/S)", ylab="stacked log(R/S)")
segments(x0= mn_pp_log_RS_repred, y0=ci_pp_log_RS_stacked[1,], y1=ci_pp_log_RS_stacked[2,], lwd=1)
segments(y0= mn_pp_log_RS_stacked, x0=ci_pp_log_RS_repred[1,], x1=ci_pp_log_RS_repred[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
# Can confirm, yes, they are essentially the same.

# get posterior predictions for predicted recruits, from full stacked posterior of model parameters and data
# get predcited recruits from stacked log(pred_r/s)
ppd <- as.data.frame(sapply(1:ncol(ppd), function(i)
  exp(pp_log_RS_stacked[,i] ) * d$total_spawners[i]
))

mn_ppd <- apply(ppd,2,mean) # get mean of predicted
#median_ppd <- apply(ppd,2,median) # get mean of predicted
ci_ppd <- apply(ppd,2,rethinking::PI,prob=0.9) # get CI of predicted
#ci_ppd50 <- apply(ppd,2,rethinking::PI,prob=0.5) # get CI of predicted
#ci_ppd10 <- apply(ppd,2,rethinking::PI,prob=0.1) # get CI of predicted

# Plot log(recruits/spawners) time series with predicted intervals
png(filename="./figures/fig_predicted_logRS_time_series.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(y=log(d$wild_recruits/d$total_spawners), x=d$brood_year, ylim=c(min(ci_pp_log_RS_stacked), max(ci_pp_log_RS_stacked)), xlab="Brood year", ylab=expression('log'[e]*'(Recruits/Spawner)'), las=1)
lines(y=mn_pp_log_RS_stacked, x=d$brood_year, col="dodger blue")
#lines(y=median_pp_log_RS, x=d$brood_year, col="firebrick")
abline(h=0, lty=2)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS_stacked[1,], rev(ci_pp_log_RS_stacked[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
#polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS_repred[1,], rev(ci_pp_log_RS_repred[2,])), col = adjustcolor('pink', alpha=0.5), border = NA)
#polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS_50[1,], rev(ci_pp_log_RS_50[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
#polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS_10[1,], rev(ci_pp_log_RS_10[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
dev.off()

# FLAG: The credible intervals are narrower for the repredicted values. May have to do 
# with including total beta*spawners, wild beta * wild spawners, and hatchery beta *
# hatchery spawners in the predicting model, which are never all three in any 
# candidate model. 

layout(mat=matrix(1:22, ncol=2, byrow=TRUE))
par(mar=c(0,0,0,0) +0.1)
for(i in 1:ncol(pp_log_RS_stacked)) {
  plot(density(pp_log_RS_stacked[,i]))
  lines(density(unlist(pp_log_RS_repred[,i])), col="pink")
  abline(v=log(d$wild_recruits[i]/d$total_spawners[i]))
}
layout(mat=matrix(1))

# Plot data with predicted intervals--------
plot(d$wild_recruits ~ d$total_spawners , ylim=c(min(ci_ppd), max(c(ci_ppd, d$wild_recruits))))
points(x=d$total_spawners, y=mn_ppd, add=TRUE, col="dodger blue")
segments(x0= d$total_spawners, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1, col="dodger blue")

# Plot recruits time series with predicted intervals
# png(filename="./figures/fig_predicted_R_time_series.png", width=1200, height=800, pointsize = 30)
# par(mar=c(4,4,0,0) +0.1)
# plot(y=d$wild_recruits, x=d$brood_year, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Brood year", ylab="Recruits")
# lines(y=mn_ppd, x=d$brood_year, col="dodger blue")
# lines(y=median_ppd, x=d$brood_year, col="firebrick")
# abline(h=0, lty=2)
# polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd[1,], rev(ci_ppd[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
# polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd50[1,], rev(ci_ppd50[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
# polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd10[1,], rev(ci_ppd10[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
# dev.off()


# Model fit --------
# Check distribution of posterior 
rethinking::dens(ppd[1], xlim=c(0,50000))
abline(v=mn_ppd[1], col="dodgerblue")
abline(v=median_ppd[1], col="firebrick")
polygon(x=c(ci_ppd[,1], rev(ci_ppd[,1])), y=c(0,0,1,1), col=adjustcolor("gray", alpha=0.5), border=NA)
polygon(x=c(ci_ppd50[,1], rev(ci_ppd50[,1])), y=c(0,0,1,1), col=adjustcolor("gray", alpha=0.5), border=NA)
polygon(x=c(ci_ppd10[,1], rev(ci_ppd10[,1])), y=c(0,0,1,1), col=adjustcolor("gray", alpha=0.5), border=NA)

# plot predicted vs observed recruits
png(filename="./figures/fig_predicted~observed_R.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(mn_ppd ~ d$wild_recruits, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Observed recruits", ylab="Predicted recruits")
segments(x0= d$wild_recruits, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()

# get R2 value
cor(log(d$wild_recruits/d$total_spawners), mn_pp_log_RS_stacked)^2
cor(d$wild_recruits, mn_ppd)^2

# Plot predicted vs observed log(recruits/spawner)
png(filename="./figures/fig_predicted~observed_log_RS.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(mn_pp_log_RS ~ log(d$wild_recruits/d$total_spawners), ylim=c(min(ci_pp_log_RS), max(ci_pp_log_RS)), xlab=expression('Observed log'[e]*'(Recruits/Spawner)'), ylab=expression('Predicted log'[e]*'(Recruits/Spawner)'))
segments(x0= log(d$wild_recruits/d$total_spawners), y0=ci_pp_log_RS[1,], y1=ci_pp_log_RS[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()

# Look at residuals
resid_logRS <- log(d$wild_recruits/d$total_spawners) - mn_pp_log_RS_stacked

# Check normality of residuals 
# histograms
png("./figures/fig_resid_hist.png",  width=1200, height=800, pointsize = 30)
hist(resid_logRS, xlab="Residuals (observed - predicted)", freq=FALSE)
#add normal curve
# curve(dnorm, add = TRUE)
dev.off()

# QQ plot
png("./figures/fig_QQ_resid.png",  width=1200, height=800, pointsize = 30)
qqnorm(resid_logRS)
qqline(resid_logRS)
dev.off() 

# PP plot
#get probability distribution for residuals
probDist <- pnorm(resid_logRS)
#create PP plot
png("./figures/fig_PP_resid.png",  width=1200, height=800, pointsize = 30)
plot(ppoints(length(resid_logRS)), sort(probDist), main = "PP Plot", xlab = "Observed Probability", ylab = "Expected Probability")
#add diagonal line
abline(0,1)
dev.off()

# Residuals ~ observed
png("./figures/fig_resid~obs.png",  width=1200, height=800, pointsize = 30)
plot(resid_logRS ~ d$wild_recruits, ylab="Residuals", xlab="Observed recruits") 
dev.off()

# Residuals ~ predicted
png("./figures/fig_resid_pred.png",  width=1200, height=800, pointsize = 30)
plot(resid_logRS ~ mn_pp_log_RS_stacked, ylab="Residuals", xlab="Predicted") 
dev.off()

# Residuals ~ year
png("./figures/fig_resid_year.png",  width=1200, height=800, pointsize = 30)
plot(resid_logRS ~ d$brood_year, ylab="Residuals", xlab="Brood year") 
dev.off()

# Check for autocorrelation in residuals 
png("./figures/fig_ACF_no_AR.png",  width=1200, height=800, pointsize = 30)
acf(resid_logRS)
dev.off()
png("./figures/fig_PACF_no_AR.png",  width=1200, height=800, pointsize = 30)
pacf(resid_logRS)
dev.off()

# Figures showing change in recruitment with different august rearing flows ------------

# equations: pred_log_RS = lnalpha - beta * S + b1 * ocean_surv + b2 * aug_mean_flow + b3 * sep_dec_max_flow + b4 * ice_days + b5 * aug_mean_flow_rear

# Get mean and 90% confidence intervals for coefficient
# Get vector of mean_aug_flow to predict for
pred_flow <- seq(min(d$aug_mean_flow_rear), max(d$aug_mean_flow_rear), length.out = 1000)

# write function to calculate log(R/S) from each run of the model for a series of flows (one beta)
# pred_mean_sp <- function(aug_flow) log(post$alpha) - post$beta * mean(d$total_spawners) + post$b5 * aug_flow
# pred_25_sp <- function(aug_flow) log(post$alpha) - post$beta * quantile(d$total_spawners, 0.25) + post$b5 * aug_flow
# pred_75_sp <- function(aug_flow) log(post$alpha) - post$beta * quantile(d$total_spawners, 0.75) + post$b5 * aug_flow
pred_mean_sp <- function(aug_flow) log(post$alpha) - post$beta_fix * mean(d$total_spawners) + post$b5 * aug_flow
pred_25_sp <- function(aug_flow) log(post$alpha) - post$beta_fix * quantile(d$total_spawners, 0.25) + post$b5 * aug_flow
pred_75_sp <- function(aug_flow) log(post$alpha) - post$beta_fix * quantile(d$total_spawners, 0.75) + post$b5 * aug_flow


# write function to calculate a log(R/S) from each run of the model for a series of flows (two betas) -------
# pred_mean_sp <- function(aug_flow) log(post$alpha) - post$betaW * mean(d$wild_spawners) - post$betaH * mean(d$hatchery_spawners) + post$b5 * aug_flow
# pred_25_sp <- function(aug_flow) log(post$alpha) - post$betaW * quantile(d$wild_spawners, 0.25) - post$betaH * quantile(d$hatchery_spawners, 0.25) + post$b5 * aug_flow
# pred_75_sp <- function(aug_flow) log(post$alpha) - post$betaW * quantile(d$wild_spawners, 0.75) - post$betaH * quantile(d$hatchery_spawners, 0.75) + post$b5 * aug_flow

# generate predictions for a series of flows
pred_logRS_mean <- sapply(pred_flow, pred_mean_sp)
pred_logRS_25 <- sapply(pred_flow, pred_25_sp)
pred_logRS_75 <- sapply(pred_flow, pred_75_sp)

pred_mean_mean <- apply(pred_logRS_mean, 2, mean)
pred_mean_HPDI <- apply(pred_logRS_mean, 2, rethinking::HPDI, prob=0.9)
pred_25_mean <- apply(pred_logRS_25, 2, mean)
pred_25_HPDI <- apply(pred_logRS_25, 2, rethinking::HPDI, prob=0.9)
pred_75_mean <- apply(pred_logRS_75, 2, mean)
pred_75_HPDI <- apply(pred_logRS_75, 2, rethinking::HPDI, prob=0.9)

# get x intercept: flow when log(R/S) is 0 (replacement)
xint <- pred_flow[which.min(abs(pred_mean_mean))]
#xint <- -(log(mean(post$alpha)) - mean(post$beta) * mean(d$total_spawners)) / mean(post$b5)
# get unscaled x intercept
xint_unscaled <- DMwR::unscale(vals=xint, norm.data=scale(d_unscaled$aug_mean_flow_rear))

# get value of x where log(R/S) is log(1.43) (sustain 30% fishery harvest) ------------
flow_30perc_harv <- pred_flow[which.min(abs(pred_mean_mean-log(1.43)))]
flow_30perc_harv_unscaled <- DMwR::unscale(vals=flow_30perc_harv, norm.data=scale(d_unscaled$aug_mean_flow_rear))

# Make vectors for unscaled mean aug flow axis
pred_flow_unscaled <- DMwR::unscale(vals=pred_flow, norm.data=scale(d_unscaled$aug_mean_flow_rear))

# get labels for second R/S y axis
sec_yax <- c(0.5, c(1,2,4,6,8,10,12,14))

# Get all the flow data
fd <- hy_daily_flows(station_number= "08LG006")
fd$year <- year(fd$Date)
fd$month <- month(fd$Date)
fd$decade <- round(fd$year, digits = -1)
table(fd$year, fd$month)
fd_check <- fd[!is.na(fd$Value), ] # some values are NA
write.csv(table(fd_check$year, fd_check$month), "available_flow_data_nicola_month_year.csv") 
# add period column for graphing density of flows for different time periods
# fd$period <- ifelse(fd$year<=1920, 1, 
#                     ifelse(fd$year <= 1968, 2,
#                     ifelse(fd$year <= 1979, 3,
#                     ifelse(fd$year <= 1991, 4, 
#                     ifelse(fd$year <= 2003, 5, 6)))))

fd$period <- ifelse(fd$year<=1920, 1, 
                    ifelse(fd$year <= 1974, 2, 
                           ifelse(fd$year <= 1991, 3, 4)))
# check number of observations in each period
table(fd$period[fd$month==8])
str(fd)
fd1 <- fd %>% group_by(period) %>% mutate(period_name = paste0(min(year), "-", max(year), " (", n_distinct(year), " yrs)")) %>% ungroup()



# summarise for august only
fd2 <- fd1 %>% filter(month==8) %>% group_by(year, period, month) %>% summarise(mean_aug_flow = mean(Value, na.rm=TRUE))
table(fd2$period) # check number of years in each period bin
# check that all augusts have full 31 days
str(fd)
dens <- density(x=fd1$Value[fd1$month==8 ], na.rm=TRUE)
#cols <- c("darkblue", "dodgerblue", "green4", "darkgoldenrod1", "chocolate2", "firebrick")
#cols <- c("darkblue", "green4", "darkgoldenrod1", "firebrick")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour blind palette with black
cols <- cbbPalette[c(1,3,2,7)]

# get long-term average august flows for paper
fd %>% filter(month==8) %>% summarise(avg_aug_flow = mean(Value, na.rm=TRUE))
fd %>% filter(month==8) %>% group_by(period) %>% summarise(avg_aug_flow = mean(Value, na.rm=TRUE))
(8.5-11.5) / 11.5 # calculate change in average august flows from period 1 to 4

# Set up for cumulative proportion flows
cdf1 <- ecdf(fd2$mean_aug_flow[fd2$period==1 ])
cdf2 <- ecdf(fd2$mean_aug_flow[fd2$period==2 ])
cdf3 <- ecdf(fd2$mean_aug_flow[fd2$period==3 ])
cdf4 <- ecdf(fd2$mean_aug_flow[fd2$period==4 ])

x <- seq(0, max(fd2$mean_aug_flow), 0.1)
p1 <- cdf1(x)
p2 <- cdf2(x)
p3 <- cdf3(x)
p4 <- cdf4(x)

# For % Mean Annual Discharge (MAD)
# Calculate mean annual discharge for Nicola, using only days from years with complete records 
# make table of flow observations by year
# test <- table(fd$year)
# get years which are complete 
# yrs_complete <- as.numeric(dimnames(test)[[1]])[which(as.numeric(test)>=365)]
# calculate MAD for values from complete years only
# mad <- mean(fd$Value[fd$year %in% yrs_complete], na.rm=TRUE)
# Use Ptolemy unpublished estimate of naturalized, long-term Mean annual discharge 
mad <- 29.8
# get %MAD axis labels


# Plot log(R/S) as a function of mean august flow with distribution of flows----------
png(filename = "./figures/fig_logRS_flow.png", width=8, height=8, units="in", res=300, pointsize=20)

# Obsolete: cumulative frequency curves
# plot(x, p1, type="l", col=cols[1], ylab="Cumulative proportion", main="", xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), lwd=2, las=1)
# lines(x, p2, col=cols[2], lwd=2)
# lines(x, p3, col=cols[3], lwd=2)
# lines(x, p4, col=cols[4], lwd=2)
# abline(h=0.5, lty=3)

layout(matrix(c(1,2,2), nrow=3, ncol=1, byrow = TRUE)) # layout for multi-panel plot

par(mar=c(0.1,4,4,4), bty="L") # plot margins for upper panel

# Add density plots
plot(density(x=fd2$mean_aug_flow[fd2$period==1 ], na.rm=TRUE), col=cols[1], main="", ylim=c(0,0.17), xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), lwd=2, las=1  )
for(i in 2:length(unique(fd2$period))) {
  lines(density(x=fd2$mean_aug_flow[fd2$period==i ], na.rm=TRUE), col=cols[i], lwd=2)
}

abline(v=xint_unscaled, col="gray", lty=2, lwd=1) # add line for replacement 

# add %MAD axis
axis(side=3, labels=seq(0,1, 0.1), at=seq(0,1, 0.1)* mad, las=1)
mtext("% Mean annual discharge", side=3, line=2, cex=0.7)
text(x=0, y=0.15, label="a")
legend("topright", 
       inset=c(0, 0.05),
       legend=unique(fd1$period_name),
       col=cols,
       pch="l",
       bty="n" )

#plot effect of mean aug flow (rearing) on recruitment
#plot(log(d$wild_recruits/d$total_spawners) ~ d$aug_mean_flow_rear, xlab="Mean Aug flow cms (scaled)", ylab="log(R/S)")
par(mar=c(4,4,0.1,4)) # set plot margins
# plot effect mean august flows (rearing) on log(R/S)
plot(pred_flow_unscaled, pred_mean_mean, type="l", lwd=1.4,  ylim=c(min(pred_75_HPDI),  max(pred_25_HPDI)), xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), xlab=expression("Mean August flow (m"^3*"s"^-1*")"), ylab=expression("log"[e]*"(Recruits/Spawner)"), las=1)

# Add credible intervals for mean prediction
rethinking::shade(pred_mean_HPDI, pred_flow_unscaled, col=adjustcolor(col="black", alpha=0.2) )
lines(pred_flow_unscaled, pred_25_mean, col="red", lwd=1.4)
rethinking::shade(pred_25_HPDI, pred_flow_unscaled, col=adjustcolor(col="pink", alpha=0.3))
lines(pred_flow_unscaled, pred_75_mean, col="dodgerblue", lwd=1.4)
rethinking::shade(pred_75_HPDI, pred_flow_unscaled, col=adjustcolor(col="dodgerblue", alpha=0.2))

legend("bottomright", # add legend for spawner abundance
       inset=c(0, 0),
       legend=c("75% quantile spawners", "Mean spawners", "25% quantile spawners"),
       col=c("dodgerblue", "black", "pink"),
       pch="l",
       bty="n" )

abline(h=0, lty=3, col="black") # add line for replacement
axis(side=4, labels=sec_yax, at=log(sec_yax), las=1) # add axis for recruits/spawner
mtext("Recruits/Spawner", side=4, line=2, cex=0.7) # label second axis
abline(v=xint_unscaled, col="gray", lty=2) # add vertical line at replacement flows
text(x=0, y=1.5, label="b")
# abline(v=14.35)
# abline(h=log(1.43))
dev.off()


# Plot for research summary for Nicola Basin Collaborative- Residuals of model formula without each covariate  -----------
# write a funtion to get predicted log(Recruits/Spawner) using mean estimates of covariates
predict_logRS <- function(smolt_age3_surv, aug_flow, ice_days, aug_flow_rear, spawners) {
  logRS <- log(mean(post$alpha)) - mean(post$beta) * mean(d$total_spawners) + mean(post$b1) * smolt_age3_surv + mean(post$b2) * aug_flow + mean(post$b4) * ice_days + mean(post$b5) * aug_flow_rear
return(logRS) 
}
pred_logRS_no_rearing_flows <- 1:nrow(d) # create a numeric vector the same length as the number of cohorts
# apply the function to the data, to get predicted logRS from all covariates except rearing flows
for (i in 1:nrow(d)) {
  pred_logRS_no_rearing_flows[i] <- predict_logRS(smolt_age3_surv= d$smolt_age3_surv[i], aug_flow = d$aug_mean_flow[i], ice_days=d$ice_days[i], aug_flow_rear=0, spawners=d$total_spawners[i])
} 

# For spawning flows
pred_logRS_no_spawning_flows <- 1:nrow(d)
for (i in 1:nrow(d)) {
  pred_logRS_no_spawning_flows[i] <- predict_logRS(smolt_age3_surv= d$smolt_age3_surv[i], aug_flow =0, ice_days=d$ice_days[i], aug_flow_rear=d$aug_mean_flow_rear[i], spawners=d$total_spawners[i])
} 

# for ice days
pred_logRS_no_ice_days <- 1:nrow(d)
for (i in 1:nrow(d)) {
  pred_logRS_no_ice_days[i] <- predict_logRS(smolt_age3_surv= d$smolt_age3_surv[i], aug_flow =d$aug_mean_flow[i], ice_days=0, aug_flow_rear=d$aug_mean_flow_rear[i], spawners=d$total_spawners[i])
} 

# for smolt to age 3 survival
pred_logRS_no_smolt_age3_surv <- 1:nrow(d)
for (i in 1:nrow(d)) {
  pred_logRS_no_smolt_age3_surv[i] <- predict_logRS(smolt_age3_surv= 0, aug_flow =d$aug_mean_flow[i], ice_days=d$ice_days[i], aug_flow_rear=d$aug_mean_flow_rear[i], spawners=d$total_spawners[i])
} 

# Plot for aug rearing flows, logR/S
png(filename = "./figures/fig_resid_plots.png", width=8, height=8, units="in", res=300, pointsize=13)
par(mar=c(6,6,0.1,0.1), bty="L")
layout(matrix(c(1,2,3,4), ncol=2, nrow=2, byrow=TRUE))

# same for smolt to age 3 survival
plot(x=d_unscaled$smolt_age3_survival, y=  log(d$recruits_per_spawner) - pred_logRS_no_ice_days, xlab="Smolt to age 3 survival", ylab=expression(atop("Residual log"[e]*"(Recruits/Spawner)", "without smolt to age 3 survival")), las=1, cex=2)
text(x=0.12, y=-1.5, label="a", col="gray")
# same for spawning flows
plot(x=d_unscaled$aug_mean_flow, y=  log(d$recruits_per_spawner) - pred_logRS_no_spawning_flows, xlab=expression("Mean August flow during spawning (m"^3*"s"^-1*")"), ylab=expression(atop("Residual log"[e]*"(Recruits/Spawner)", "without Aug. spawning flow effect")), las=1, cex=2)
text(x=25, y=-1.5, label="b", col="gray")
# same for ice days
plot(x=d_unscaled$ice_days, y=  log(d$recruits_per_spawner) - pred_logRS_no_ice_days, xlab="Ice days", ylab=expression(atop("Residual log"[e]*"(Recruits/Spawner)", "without ice days effect")), las=1, cex=2)
text(x=110, y=-1.5, label="c", col="gray")
# august rearing flows
plot(x=d_unscaled$aug_mean_flow_rear, y=  log(d$recruits_per_spawner) - pred_logRS_no_rearing_flows, xlab=expression("Mean August flow during rearing (m"^3*"s"^-1*")"), ylab=expression(atop("Residual log"[e]*"(Recruits/Spawner)", "without Aug. rearing flow effect")), las=1, cex=2)
#abline(lm(d$recruits_per_spawner - exp(pred_logRS_no_rearing_flows) ~ d_unscaled$aug_mean_flow_rear))
text(x=25, y=-1.75, label="d", col="gray")
dev.off()

# Plot residuals of observed and mean ricker predictions without any covariates - for supplemental -------
predicted_ricker <- d$total_spawners*mean(post$alpha)*exp(-mean(post$beta_fix)*d$total_spawners) # get predicted mean spawners
png(filename = "./figures/fig_resid_ricker_plots.png", width=8, height=8, units="in", res=300, pointsize=17)
layout(matrix(c(1,2,3,4,5,6), ncol=2, nrow=3, byrow=TRUE))
par(mar=c(4,4,0.5,0.1), bty="L")
# Standard ricker with residuals
plot(y=d$wild_recruits, x=d$total_spawners, ylab="Wild recruits", xlab="Total spawners")
curve(x*mean(post$alpha)*exp(-mean(post$beta_fix)*x ), from=0, to=max(d$total_spawners), add=TRUE)
segments(x0=d$total_spawners, x1=d$total_spawners, y0=predicted_ricker, y1=d$wild_recruits, col="dodgerblue")
text(x=15000, y=15500, label="a", col="gray")
# Smolt to age 3 survival
plot(x=d_unscaled$smolt_age3_survival, y=d$wild_recruits - predicted_ricker , ylab="Residual recruits", xlab="Smolt to age 3 survival", cex=1.5)
#text(x=d_unscaled$smolt_age3_survival, y=d$wild_recruits - predicted_ricker, label=d$brood_year )
text(x=0.1, y=10000, label="b", col="gray")

# Aug flow spawning
plot(x=d_unscaled$aug_mean_flow, y=d$wild_recruits - predicted_ricker, ylab="Residual recruits", xlab=expression("Mean August flow during spawning (m"^3*"s"^-1*")"), cex=1.5)
#text(x=d_unscaled$aug_mean_flow, y=d$wild_recruits - predicted_ricker, label=d$brood_year )
text(x=25, y=10000, label="c", col="gray")

# Fall flood max
plot(x=d_unscaled$sep_dec_max_flow, y=d$wild_recruits - predicted_ricker , ylab="Residual recruits", xlab=expression("Max flood Sep-Dec during incubation (m"^3*"s"^-1*")"), cex=1.5)
#text(x=d_unscaled$sep_dec_max_flow, y=d$wild_recruits - predicted_ricker, label=d$brood_year )
text(x=175, y=10000, label="d", col="gray")

# Ice days
plot(x=d_unscaled$ice_days, y=d$wild_recruits - predicted_ricker, ylab="Residual recruits", xlab="Ice days", cex=1.5)
#text(x=d_unscaled$ice_days, y=d$wild_recruits - predicted_ricker, label=d$brood_year )
text(x=110, y=10000, label="e", col="gray")

# Aug flow rearing
plot(x=d_unscaled$aug_mean_flow_rear, y=d$wild_recruits - predicted_ricker, ylab="Residual recruits", xlab=expression("Mean August flow during rearing (m"^3*"s"^-1*")"), cex=1.5)
#text(x=d_unscaled$aug_mean_flow_rear, y=d$wild_recruits - predicted_ricker, label=d$brood_year )
text(x=25, y=10000, label="f", col="gray")

dev.off()

# Plot for IDEAS talk and research summary - Recruits / spawner and august flow - simple ---------
flows_periods1_4 <- c(median(fd2$mean_aug_flow[fd2$period==1]), median(fd2$mean_aug_flow[fd2$period==4]))
flows_periods1_4_scale <- (flows_periods1_4- mean(d_unscaled$aug_mean_flow_rear)) / sd(d_unscaled$aug_mean_flow_rear)
pred_periods1_4 <- sapply(flows_periods1_4_scale, pred_mean_sp)
mean_pred_periods1_4 <- apply(pred_periods1_4, 2, mean)

png(filename = "./figures/fig_logRS_flow_simple_IDEAS.png", width=6, height=6, units="in", res=300, pointsize=20)
par(mar=c(4,4,0.3,0.3),  bty="L")
plot(pred_flow_unscaled, pred_mean_mean, type="l", lwd=1.4,  ylim=c(min(pred_mean_HPDI),  max(pred_mean_HPDI)), xlim=c(min(d_unscaled$aug_mean_flow_rear),max(d_unscaled$aug_mean_flow_rear)), xlab=expression("Mean Aug flow (m"^3*"s"^-1*")"), ylab="Recruits/Spawner", yaxt="n", las=1)
abline(h=0, lty=3)
rethinking::shade(pred_mean_HPDI, pred_flow_unscaled, col=adjustcolor(col="black", alpha=0.2) )
axis(side=2, labels=sec_yax, at=log(sec_yax), las=1)
#abline(v=xint_unscaled, col="gray", lty=4, lwd=2)
# points(y= mean_pred_periods1_4, x= flows_periods1_4, pch= 19, cex=2, col=c("blue", "red"))
#text(y=rep(1,2), x=flows_periods1_4, labels= list(bquote(.(round(flows_periods1_4[1],1)) ~ m^3 ~ s^-1), bquote(.(round(flows_periods1_4[2],1)) ~ m^3 ~ s^-1)), offset=1, col=c("blue", "red") )
#arrows(x0=flows_periods1_4[1], x1= flows_periods1_4[2], y0=1, y1=1)
dev.off()
exp(mean_pred_periods1_4)
xint_unscaled

 # Calculate environmental effects on recruitment for paper results-------
# get posterior predictions beta terms
ef <- post[, grep("b[[:digit:]]", colnames(post))]
mn_ef <- apply(ef,2,mean) # get mean of predicted
ci95_ef <- apply(ef,2,rethinking::PI,prob=0.95) # get CI of predicted
ci80_ef <- apply(ef,2,rethinking::PI,prob=0.8) # get CI of predicted
mn_ef

# Write function to convert any aribitrary flow into scaled according to actual values
faux_scale <- function(x, scale_by) {
  (x -  mean(scale_by)) / sd(scale_by)
}

# test
faux_scale(x= 10.3, scale_by=d_unscaled$aug_mean_flow_rear)
# check that qauntiles are equal
quantile(d$smolt_age3_survival, 0.25)
faux_scale(x=quantile(d_unscaled$smolt_age3_survival, 0.25), scale_by=d_unscaled$smolt_age3_survival)

# write function to calculate recruits from each run of the model for a series of flows (one beta)
predict_recruits <- function(smolt_age3_surv, aug_flow, aug_flow_rear, ice_days, spawners) {
  logRS <- log(mean(post$alpha)) - mean(post$beta) * mean(d$total_spawners) + mean(post$b1) * smolt_age3_surv + mean(post$b2) * aug_flow + mean(post$b4) * ice_days + mean(post$b5) * aug_flow_rear 
  RS <- exp(logRS)
  R <- RS * spawners
  return(c(RS, R))}

# predict recruits at mean and 50% below mean rearing flows
mean(d_unscaled$aug_mean_flow_rear)
mean(d_unscaled$aug_mean_flow_rear) - 0.5 * mean(d_unscaled$aug_mean_flow_rear)

pred1 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = 0, 
                 aug_flow_rear= faux_scale(mean(d_unscaled$aug_mean_flow_rear), d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# predict recruits at lower rearing flows
pred2 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = 0, 
                 aug_flow_rear= faux_scale( mean(d_unscaled$aug_mean_flow_rear) - 0.5 * mean(d_unscaled$aug_mean_flow_rear), d_unscaled$aug_mean_flow_rear),

                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1

# Caculate change in recruitment for spawning flows 50% of average
pred1 <- predict_recruits(smolt_age3_surv=0,
                          aug_flow = faux_scale(mean(d_unscaled$aug_mean_flow), d_unscaled$aug_mean_flow), 
                          aug_flow_rear= 0, 
                          ice_days = 0,
                          spawners = mean(d$total_spawners))
# predict recruits at lower rearing flows
pred2 <- predict_recruits(smolt_age3_surv=0,
                          aug_flow = faux_scale( mean(d_unscaled$aug_mean_flow) - 0.5 * mean(d_unscaled$aug_mean_flow), d_unscaled$aug_mean_flow), 
                          aug_flow_rear= 0,
                          ice_days = 0,
                          spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1

# Calculate change in recruitment for rearing and spawning flows 50% of average
pred1 <- predict_recruits(smolt_age3_surv=0,
                          aug_flow = faux_scale(mean(d_unscaled$aug_mean_flow), d_unscaled$aug_mean_flow), 
                          aug_flow_rear= faux_scale(mean(d_unscaled$aug_mean_flow_rear), d_unscaled$aug_mean_flow_rear), 
                          ice_days = 0,
                          spawners = mean(d$total_spawners))
# predict recruits at lower rearing flows
pred2 <- predict_recruits(smolt_age3_surv=0,
                          aug_flow = faux_scale( mean(d_unscaled$aug_mean_flow) - 0.5 * mean(d_unscaled$aug_mean_flow), d_unscaled$aug_mean_flow), 
                          aug_flow_rear= faux_scale( mean(d_unscaled$aug_mean_flow_rear) - 0.5 * mean(d_unscaled$aug_mean_flow_rear), d_unscaled$aug_mean_flow_rear),
                          ice_days = 0,
                          spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1

# predict recruits if flows were at kosakoski and hamilton levels for spawning and rearing
predict_recruits(smolt_age3_surv=0,
                 aug_flow = faux_scale(5.66, scale_by=d_unscaled$aug_mean_flow_rear), 
                 aug_flow_rear= faux_scale(5.66, scale_by=d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# add poor ocean survival (25% quantile)
predict_recruits(smolt_age3_surv=quantile(d$smolt_age3_survival, 0.25),
                 aug_flow = faux_scale(5.66, scale_by=d_unscaled$aug_mean_flow_rear), 
                 aug_flow_rear= faux_scale(5.66, scale_by=d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))

# calculate difference in recruits for aug flow rearing effect in periods 1 and 4
# predict recruits for mean flow from period 1
pred1 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = 0, 
                 aug_flow_rear= faux_scale(flows_periods1_4[1], scale_by=d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# predict recruits for mean flow from period 4
pred2 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = 0, 
                 aug_flow_rear= faux_scale(flows_periods1_4[2], scale_by=d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1

# Calculate difference in recruits for aug flow spawning and rearing effect in periods 1 and 4
pred1 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = faux_scale(flows_periods1_4[1], scale_by=d_unscaled$aug_mean_flow_rear), 
                 aug_flow_rear= faux_scale(flows_periods1_4[1], scale_by=d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# predict recruits for mean flow from period 4
pred2 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = faux_scale(flows_periods1_4[2], scale_by=d_unscaled$aug_mean_flow_rear), 
                 aug_flow_rear= faux_scale(flows_periods1_4[2], scale_by=d_unscaled$aug_mean_flow_rear), 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1

# Calculate difference in recruits for aug flow spawning effect in periods 1 and 4
pred1 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = faux_scale(flows_periods1_4[1], scale_by=d_unscaled$aug_mean_flow_rear), 
                 aug_flow_rear= 0, 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# predict recruits for mean flow from period 4
pred2 <-predict_recruits(smolt_age3_surv=0,
                 aug_flow = faux_scale(flows_periods1_4[2], scale_by=d_unscaled$aug_mean_flow_rear), 
                 aug_flow_rear= 0, 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1

# Calculate difference in recruitment for ice days effect
pred1 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = 0, 
                 aug_flow_rear= 0, 
                 ice_days = 0,
                 spawners = mean(d$total_spawners))
pred2 <- predict_recruits(smolt_age3_surv=0,
                 aug_flow = 0, 
                 aug_flow_rear= 0, 
                 ice_days = faux_scale(mean(d_unscaled$ice_days) + 10, d_unscaled$ice_days),
                 spawners = mean(d$total_spawners))
# calculate percent chage
(pred2 - pred1) / pred1


# Plot effect sizes for paper: covariates -------
library(bayesplot)
fig_effect_sizes <- mcmc_areas(post, pars=c("b5", "b4","b3", "b2", "b1"), prob=0.8) + 
  xlab(expression("Effect on log"[e]*"(Recruits/Spawner)")) +
  ylab("Parameter") +
  geom_vline(aes(xintercept=0)) +
  scale_y_discrete(labels=c("Aug. flow rearing (b5)", "Ice days (b4)", "Max fall flood (b3)", "Aug. flow spawning (b2)", "Smolt to age 3 survival (b1)")) +
  theme_classic()
fig_effect_sizes
ggsave("./figures/fig_effect_sizes.png", fig_effect_sizes, width=6, height=6 )

# plot effect sizes for paper: betas
fig_effect_sizes_betas <- mcmc_areas(post, pars=c("beta", "beta_fix", "betaW", "betaW_fix", "betaH", "betaH_fix"), prob=0.8) + 
  xlab(expression("Effect on log"[e]*"(Recruits/Spawner)")) +
  ylab("Parameter") +
  geom_vline(aes(xintercept=0)) +
  theme_classic()
fig_effect_sizes_betas
ggsave("./figures/fig_effect_sizes_betas.png", fig_effect_sizes, width=6, height=4 )



# For IDEAS
png(filename = "./figures/fig_effect_sizes_IDEAS.png", width=12, height=5, units="in", res=300, pointsize=25)
layout(matrix(c(1), nrow=1, ncol=1, byrow = TRUE))
par(mar=c(0.3,4,0.3,0.3)+0.2,  bty="L")
plot(x=1:4, ylim=c(min(ci95_ef), max(ci95_ef)), xlim=c(1,4), ylab=expression("Effect on log"[e]*"(Recruits/Spawner)"), xlab="", xaxt="n")
abline(h=0)
segments(y0=ci95_ef[1,][c(2:4,1)], y1=ci95_ef[2,][c(2:4,1)], x0=1:4, x1=1:4)
segments(y0=ci80_ef[1,][c(2:4,1)], y1=ci80_ef[2,][c(2:4,1)], x0=1:4, x1=1:4, lwd=10, col=c("green", "red", "green","gray"))
points(x=as.factor(names(mn_ef)), y=mn_ef[c(2:4,1)], type="p", cex=3, pch=19, ylim=c(min(ci95_ef), max(ci95_ef)))
dev.off()

# Figure for changing hydrology --------
# setup for circle graph of
unique(fd$Parameter)
fd$yday <- yday(fd$Date)

fd_avg_prd <- fd %>% group_by(period, yday) %>% summarise(avg_flow = mean(Value, na.rm=TRUE), count_years = n() ) # get daily average for each period
fd_avg_all <- fd %>% group_by(yday) %>% summarise(hist_avg_flow = mean(Value, na.rm=TRUE)) # get average for all time
fd_dif <- merge(fd_avg_prd, fd_avg_all, by=c("yday"))
fd_dif$perc_change <- (fd_dif$avg_flow - fd_dif$hist_avg_flow) / fd_dif$hist_avg_flow *100
periods_sum <- fd1 %>% group_by(period, period_name) %>% summarise()
periods_sum$num_years <- as.numeric(substr(periods_sum$period_name, 12,13 ))
fd_dif <- merge(fd_dif, periods_sum, by="period", all.x=TRUE)
fd_dif$perc_yrs_data <- fd_dif$count_years / fd_dif$num_years * 100

# join period name to flow data 
fd <- merge(fd, periods_sum, by="period", all.x=TRUE) 


# calculate changes in summer yield for periods 1 and 4 and percent change - for paper ------------
# get average summer yield by period
fd %>% filter(month %in% c(8)) %>% 
  group_by(year, period) %>% 
  summarise(summer_yield = sum(Value * 86400, na.rm=TRUE)) %>%
  group_by(period) %>%
  summarise(mean_summer_yield = mean(summer_yield, na.rm=TRUE))

# # long to wide format to get percent change between periods
# fd_change <- fd_avg_prd %>% pivot_wider(names_from=period, names_prefix= "prd", values_from= avg_flow )
# fd_change$dif4_2 <- (fd_change$prd4 - fd_change$prd2) / fd_change$prd2 * 100
# fd_change$dif4_1 <- (fd_change$prd4 - fd_change$prd1) / fd_change$prd1 * 100
# fd_change$dif4_3 <- (fd_change$prd4 - fd_change$prd3) / fd_change$prd3 * 100
# # back to long format 
# fd_change_l <- fd_change[,c(1,6:8)] %>% pivot_longer(cols=c(dif4_2, dif4_1, dif4_3), names_to = "prd_change", values_to = "perc_change")

# Figure with august flows in boxplots by year - for supplemental--------
x_brks <- seq(min(fd$year), max(fd$year), 1) # get year breaks
x_brks_2 <- seq(round(min(x_brks), -1), round(max(x_brks),-1), 5 )
png(filename = "./figures/fig_aug_flows.png", width=8, height=6, units="in", res=300, pointsize=30)
fd %>% filter(month==8) %>% 
  ggplot(., aes(y=Value, x=year, group=year)) + 
  geom_boxplot() + 
  stat_summary(fun.y="mean", geom="point", colour="dodgerblue") +
  ylab(expression("Daily discharge in August (m"^3*"s"^-1*")"))+
  xlab("Year") +
  scale_x_continuous(breaks=x_brks_2, labels=x_brks_2, limits=c(min(x_brks)-1, max(x_brks)+1), expand=c(0,0), minor_breaks=x_brks_2) +
  theme_bw() +
  geom_hline(aes(yintercept=0.15*mad), linetype=2, stroke=1.2, colour="orange") +
  geom_hline(aes(yintercept=0))+
  coord_cartesian(expand=FALSE)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        panel.grid.minor.x=element_line(colour=adjustcolor("gray", alpha=0.5)))
dev.off()

# Figure with time series of each variable by year for supplemental-------
png(filename = "./figures/fig_unscaled_variables_time_series.png", width=8, height=8, units="in", res=300, pointsize=15)
layout(matrix(c(1,2,3,4,5,6), ncol=2, nrow=3, byrow=TRUE))
par(mar=c(4,5,0.5,0.1), bty="L")

# Smolt to age 3 survival
plot(y=d_unscaled$smolt_age3_survival, x=d_unscaled$brood_year , ylab="Smolt-to-age 3 survival", xlab="Brood year", cex=1.5, type="b")
text(y=0.12, x=2010, label="a", col="gray")

# Aug flow spawning
plot(y=d_unscaled$aug_mean_flow, x=d_unscaled$brood_year , ylab=expression("Mean Aug flow, spawning (m"^3*"s"^-1*")"), xlab="Brood year", cex=1.5, type="b")
text(y=25, x=2010, label="b", col="gray")

# Fall flood max
plot(y=d_unscaled$sep_dec_max_flow, x=d_unscaled$brood_year , ylab=expression("Max flood Sep-Dec (m"^3*"s"^-1*")"), xlab="Brood year", cex=1.5, type="b")
text(y=200, x=2010, label="c", col="gray")

# Ice days
plot(y=d_unscaled$ice_days, x=d_unscaled$brood_year ,ylab="Ice days", xlab="Brood year", cex=1.5, type="b")
text(y=115, x=2010, label="d", col="gray")

# Aug flow rearing
plot(y=d_unscaled$aug_mean_flow_rear, x=d_unscaled$brood_year, ylab=expression("Mean Aug flow, rearing (m"^3*"s"^-1*")"), xlab="Brood year", cex=1.5, type="b")
text(y=25, x=2010, label="e", col="gray")

dev.off()

# calculate mean and sd of each environmental covariate for paper
vars_mean_sd<- d_unscaled %>% select(smolt_age3_survival, aug_mean_flow, sep_dec_max_flow, ice_days, aug_mean_flow_rear) %>% 
  summarise_all(.funs=list(mean, sd))
vars_mean_sd

# setup for labels
days_month <- as.vector(table(fd$month, fd$year)[,"1970"])
month_breaks <- cumsum(days_month)-31 + 1
month_labels <- format(ISOdatetime(2000,1:12,1,0,0,0),"%b")

# days_month_water_yr <- days_month[c(9:12, 1:8)]
# month_breaks_water_yr <- cumsum(days_month_water_yr)-30+1
# month_labels_water_yr <- format(ISOdatetime(2000,c(9:12,1:8),1,0,0,0),"%b")
unique(fd_dif$perc_yrs_data)
range(fd_dif$perc_change, na.rm=TRUE)

# Hydrographs by period
fig_hyd_period <- ggplot(data=fd_dif[fd_dif$count_years>5, ], aes(y=avg_flow, x=yday, group=period, colour=factor(period_name))) +
  geom_line( lineend="round") +
  geom_line(data=fd, aes(y=Value, x=yday, group=interaction(period_name, year)), alpha=0.1) +
  scale_colour_manual(values=cols,  name="Period") + 
  #scale_size_continuous(range=c(0.1,2), name="Percent of years in\nperiod with data on day") +
  #scale_y_continuous(limits=c(-200,max(fd_change_l$perc_change)), expand=c(0,0), breaks=NULL) +
  #scale_y_continuous(limits=c(-300,100), expand=c(0,0)) +
  scale_x_continuous(breaks=month_breaks, labels=month_labels, minor_breaks=NULL, expand=c(0,0)) +
  #scale_y_continuous(breaks=seq(-100,300,50)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("") +
  ylab(expression("\nFlow in Nicola River (m"^3*"s"^-1*")")) +
  theme_classic() +
  theme(panel.grid.major.x = element_line(colour="gray", linetype=3),
        legend.position = c(0.2, 0.8),
        legend.background = element_blank(),
        legend.box = "horizontal")
fig_hyd_period
ggsave("./figures/fig_hyd_period.png", fig_hyd_period, width=10, height=5 )

# zoom in on August to December
fig_hyd_aug <- ggplot(data=fd_dif[fd_dif$count_years>5, ], aes(y=avg_flow, x=yday, group=period_name)) +
  geom_line(aes(colour=factor(period_name)), lineend="round") +
  scale_colour_manual(values=rep(cols,2), guide=FALSE) + 
  #scale_size_continuous(range=c(0.1,2), name="Percent of years in\nperiod with data on day") +
  #scale_y_continuous(limits=c(-200,max(fd_change_l$perc_change)), expand=c(0,0), breaks=NULL) +
  #scale_y_continuous(limits=c(-300,100), expand=c(0,0)) +
  scale_x_continuous(breaks=month_breaks, labels=month_labels, minor_breaks=NULL, expand=c(0,0), limits=c(yday("2000-07-31"), yday("2000-12-31"))) +
  scale_y_continuous(limits=c(0,30), expand=c(0,0)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(colour="gray", linetype=3),
        legend.position = c(0.2, 0.8),
        legend.background = element_blank(),
        rect = element_rect(fill = "transparent", colour=NA),
        legend.box = "horizontal")
fig_hyd_aug
ggsave("./figures/fig_hyd_aug.png", fig_hyd_aug, width=4, height=2)

# Figure with deviations from historic average flows by period. Remove days where there is less than 5 years of data
fig_hyd_change_hist <-ggplot(data=fd_dif[fd_dif$count_years>5, ], aes(y=perc_change, x=yday, group=period)) +
  #geom_col(aes(fill=perc_change, group=period), colour="white"width=2) +
  geom_ribbon(aes(ymin=0, ymax=perc_change, fill=factor(period_name)), alpha=0.3)+
  geom_line(aes(colour=factor(period_name)), lineend="round") +
  #scale_fill_gradientn(colours=c("black","red", "white", "green", "blue"), values=c(0,0.12, scales::rescale(x=0, to=c(0,1), from=range(fd_change_l$perc_change[fd_change_l$prd_change =="dif4_1"], na.rm=TRUE)), 0.5, 1), breaks=seq(-100,250,50), labels=as.character(seq(-100,250,50)), limits=c(-100,250), name="Percent difference\n from historic\naverage daily flow") +
  #scale_fill_gradient2(high="white", mid="gray", low="black", midpoint=0,  breaks=seq(-50,150,50), labels=as.character(seq(-50,150,50)), limits=c(-50,150), name="Percent difference\n from historic\naverage daily flow") +
  #coord_polar() + 
  scale_fill_manual(values=cols, guide=FALSE) + 
  scale_colour_manual(values=cols, guide=FALSE) + 
  #scale_size_continuous(range=c(0.1,2), name="Percent of years in\nperiod with data on day") +
  #scale_y_continuous(limits=c(-200,max(fd_change_l$perc_change)), expand=c(0,0), breaks=NULL) +
  #scale_y_continuous(limits=c(-300,100), expand=c(0,0)) +
  scale_x_continuous(breaks=month_breaks, labels=month_labels, minor_breaks=NULL, expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-100,300,50)) +
  geom_hline(aes(yintercept=0)) +
  xlab("Day of year") +
  ylab("Percent difference of daily average flow for\nperiod and daily average from 1911-2014") +
  theme_classic() +
  theme(panel.grid.major.x = element_line(colour="gray", linetype=3),
        legend.position = c(0.2, 0.8),
        legend.background = element_blank(),
        legend.box = "horizontal")
fig_hyd_change_hist
ggsave("./figures/fig_hyd_change_hist.png", fig_hyd_change_hist, width=10, height=5 )

# check min, max
range(fd_change$dif4_2)
range(fd_change$dif4_1, na.rm=TRUE)

# Look at posteriors
rethinking::dens(post$alpha)
rethinking::dens(post$beta)
rethinking::dens(post$b1)
abline(v=0)
rethinking::dens(post$b2)
abline(v=0)
rethinking::dens(post$b3)
abline(v=0)
rethinking::dens(post$b4)
abline(v=0)
rethinking::dens(post$b5)
abline(v=0)
rethinking::dens(post$tau)

# Check correlation between mean august flow and 20th quantile flow
compare <- fd %>% filter(month==8) %>% group_by(year) %>% summarise(mean= mean(Value, na.rm=TRUE), Q20 = quantile(Value, probs=0.2, na.rm=TRUE))
plot(compare$mean ~ compare$Q20, ylab="Mean August flow", xlab="20th Percentile August flow")
abline(h=10.83, lty=3)
abline(v=6, lty=2)

#### OBSOLETE ####

# Changes in carrying capacity, etc. over hydrometric flow periods------
# See Hilborn and Walters 2013 , Table 7.2 for equations 
# calculate productivity using flow from different periods - note that other environmental parameters can be dropped since mean values are 0 since they are scaled. 
#prod <- post$alpha*exp(post$b5 * BOOTSTRAP FLOW VALS HERE)
# calculate carrying capacity
CC <- log(post$alpha)/post$beta
# calculate SMSY using posterior samples
SMSY <- log(post$alpha)/post$beta * (0.5 - 0.07*log(post$alpha))

# # Plot triptych plot, min flood, mean flood, max flood
# png(filename="./figures/fig_flood_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
# par(mar=c(4,4,4,0) +0.1)
# plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max flood effects on recruitment", xlab="Spawners", ylab="Recruits")
# text( d$total_spawners, d$wild_recruits, labels=round(d$max_flow_fall, 0), adj=c(0,0), cex= 0.7)
# # Min flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * min(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
# # Mean flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# # Max flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * max(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# dev.off()
# 
# # Plot triptych plot, min, mean, max Aug flow
# png(filename="./figures/fig_aug_flow_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
# par(mar=c(4,4,4,0) +0.1)
# plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max August mean flow effects on recruitment", xlab="Spawners", ylab="Recruits")
# text( d$total_spawners, d$wild_recruits, labels=round(d$mean_flow_aug_rearing, 0), adj=c(0,0), cex= 0.7)
# # Min flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * min(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# # Mean flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# # Max flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * max(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
# dev.off()
# 
# # Plot triptych plot, min, mean, max Ocean survival anomaly
# png(filename="./figures/fig_ocean_triptych_lognormal.png", width=1200, height=800, pointsize = 25)
# par(mar=c(4,4,4,0) +0.1)
# plot(d$wild_recruits ~ d$total_spawners, main="Min, mean, and max ocean survival anomaly effects on recruitment", xlab="Spawners", ylab="Recruits")
# text( d$total_spawners, d$wild_recruits, labels=round(d$ocean_surv_anomaly, 1), adj=c(0,0), cex= 0.7)
# # Min flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * min(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="red", lwd=3, add=TRUE)
# # Mean flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="gray", lwd=3, add=TRUE)
# # Max flood
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * max(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) +
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
# dev.off()

