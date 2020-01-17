
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
library(EflowStats)
rstan_options(auto_write=TRUE)

rm(list=ls())

# read in data used in model
d <- read.csv("./data/model_data.csv")
# read in unscaled data 
d_unscaled <- read.csv("./data/model_data_unscaled.csv")
# read in data frame of posterior samples of model parameters
post <- read.csv("./data/posterior_samples.csv")

# get posterior predictions for recruits
ppd <- post[, grep("pp_R", colnames(post))]
mn_ppd <- apply(ppd,2,mean) # get mean of predicted
median_ppd <- apply(ppd,2,median) # get mean of predicted
ci_ppd <- apply(ppd,2,rethinking::PI,prob=0.9) # get CI of predicted
ci_ppd50 <- apply(ppd,2,rethinking::PI,prob=0.5) # get CI of predicted
ci_ppd10 <- apply(ppd,2,rethinking::PI,prob=0.1) # get CI of predicted

# get posterior predictions for log(recruits/spawners)
pp_log_RS <- post[, grep("pp_log_RS", colnames(post))]
mn_pp_log_RS <- apply(pp_log_RS,2,mean) # get mean of predicted
median_pp_log_RS <- apply(pp_log_RS,2,median) # get mean of predicted
ci_pp_log_RS <- apply(pp_log_RS,2,rethinking::PI,prob=0.9) # get CI of predicted
ci_pp_log_RS_50 <- apply(pp_log_RS,2,rethinking::PI,prob=0.5) # get CI of predicted
ci_pp_log_RS_10 <- apply(pp_log_RS,2,rethinking::PI,prob=0.1) # get CI of predicted


# Plot data with predicted intervals--------
plot(d$wild_recruits ~ d$total_spawners , ylim=c(min(ci_ppd), max(c(ci_ppd, d$wild_recruits))))
points(x=d$total_spawners, y=mn_ppd, add=TRUE, col="dodger blue")
segments(x0= d$total_spawners, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1, col="dodger blue")

# Plot recruits time series with predicted intervals
png(filename="./figures/fig_predicted_R_time_series.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(y=d$wild_recruits, x=d$brood_year, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Brood year", ylab="Recruits")
lines(y=mn_ppd, x=d$brood_year, col="dodger blue")
lines(y=median_ppd, x=d$brood_year, col="firebrick")
abline(h=0, lty=2)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd[1,], rev(ci_ppd[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd50[1,], rev(ci_ppd50[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_ppd10[1,], rev(ci_ppd10[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
dev.off()

# Check distribution of posterior 
rethinking::dens(ppd[1], xlim=c(0,50000))
abline(v=mn_ppd[1], col="dodgerblue")
abline(v=median_ppd[1], col="firebrick")
polygon(x=c(ci_ppd[,1], rev(ci_ppd[,1])), y=c(0,0,1,1), col=adjustcolor("gray", alpha=0.5), border=NA)
polygon(x=c(ci_ppd50[,1], rev(ci_ppd50[,1])), y=c(0,0,1,1), col=adjustcolor("gray", alpha=0.5), border=NA)
polygon(x=c(ci_ppd10[,1], rev(ci_ppd10[,1])), y=c(0,0,1,1), col=adjustcolor("gray", alpha=0.5), border=NA)

# Plot log(recruits/spawners) time series with predicted intervals
png(filename="./figures/fig_predicted_logRS_time_series.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(y=log(d$wild_recruits/d$total_spawners), x=d$brood_year, ylim=c(min(ci_pp_log_RS), max(ci_pp_log_RS)), xlab="Brood year", ylab=expression('log'[e]*'(Recruits/Spawner)'), las=1)
lines(y=mn_pp_log_RS, x=d$brood_year, col="dodger blue")
#lines(y=median_pp_log_RS, x=d$brood_year, col="firebrick")
abline(h=0, lty=2)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS[1,], rev(ci_pp_log_RS[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS_50[1,], rev(ci_pp_log_RS_50[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
polygon(x=c(d$brood_year, rev(d$brood_year)), y=c(ci_pp_log_RS_10[1,], rev(ci_pp_log_RS_10[2,])), col = adjustcolor('grey', alpha=0.5), border = NA)
dev.off()

# plot predicted vs observed recruits
png(filename="./figures/fig_predicted~observed_R.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(mn_ppd ~ d$wild_recruits, ylim=c(min(ci_ppd), max(ci_ppd)), xlab="Observed recruits", ylab="Predicted recruits")
segments(x0= d$wild_recruits, y0=ci_ppd[1,], y1=ci_ppd[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()


# plot predicted vs observed log recruits
png(filename="./figures/fig_predicted~observed_log_R.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(log(mn_ppd) ~ log(d$wild_recruits), ylim=c(min(log(ci_ppd)), max(log(ci_ppd))), xlab="log(Observed recruits)", ylab="log(Predicted recruits)")
segments(x0= log(d$wild_recruits), y0=log(ci_ppd[1,]), y1=log(ci_ppd[2,]), lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()

# get R2 value
cor(log(d$wild_recruits/d$total_spawners), mn_pp_log_RS)^2

# Plot predicted vs observed log(recruits/spawner)
png(filename="./figures/fig_predicted~observed_log_RS.png", width=1200, height=800, pointsize = 30)
par(mar=c(4,4,0,0) +0.1)
plot(mn_pp_log_RS ~ log(d$wild_recruits/d$total_spawners), ylim=c(min(ci_pp_log_RS), max(ci_pp_log_RS)), xlab="Observed log(recruits/spawner)", ylab="Predicted log(recruits/spawner)")
segments(x0= log(d$wild_recruits/d$total_spawners), y0=ci_pp_log_RS[1,], y1=ci_pp_log_RS[2,], lwd=1)
abline(b=1, a=0, lwd=2, lty=2, col="orange")
dev.off()

# Look at residuals
resid_logRS <- log(d$wild_recruits/d$total_spawners) - mn_pp_log_RS

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
plot(resid_logRS ~ mn_pp_log_RS, ylab="Residuals", xlab="Predicted") 
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
pred_mean_sp <- function(aug_flow) log(post$alpha) - post$beta * mean(d$total_spawners) + post$b5 * aug_flow
pred_25_sp <- function(aug_flow) log(post$alpha) - post$beta * quantile(d$total_spawners, 0.25) + post$b5 * aug_flow
pred_75_sp <- function(aug_flow) log(post$alpha) - post$beta * quantile(d$total_spawners, 0.75) + post$b5 * aug_flow

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
# write.csv(table(fd$year, fd$month), "decades.csv")
str(fd)
dens <- density(x=fd1$Value[fd1$month==8 ], na.rm=TRUE)
#cols <- c("darkblue", "dodgerblue", "green4", "darkgoldenrod1", "chocolate2", "firebrick")
cols <- c("darkblue", "green4", "darkgoldenrod1", "firebrick")

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


# Plot with cumulative distribution----------
png(filename = "./figures/fig_logRS_flow.png", width=8, height=11, units="in", res=300, pointsize=20)


layout(matrix(c(1,2,3,3), nrow=4, ncol=1, byrow = TRUE))
par(mar=c(0.1,4,0.1,4), bty="L")

plot(x, p1, type="l", col=cols[1], ylab="Cumulative proportion", main="", xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), lwd=2, las=1)
lines(x, p2, col=cols[2], lwd=2)
lines(x, p3, col=cols[3], lwd=2)
lines(x, p4, col=cols[4], lwd=2)

abline(v=xint_unscaled, col="gray", lty=4, lwd=2)
abline(h=0.5, lty=3)

#png(filename = "./figures/fig_logRS~flow.png", width=1200, height=1600, pointsize = 25)

#par(mfrow=c(2,1), mar=c(0.1,4,0.1,4), bty="L")
#plot(density(x=fd1$Value[fd1$month==8 & fd1$period==1 ], na.rm=TRUE), col=cols[1], main="", ylim=c(0,0.17), xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), lwd=2 #, xaxt="n"
#for(i in 2:length(unique(fd1$period))) {
#lines(density(x=fd$Value[fd1$month==8 & fd1$period==i ], na.rm=TRUE), col=cols[i], lwd=2)
#}
plot(density(x=fd2$mean_aug_flow[fd2$period==1 ], na.rm=TRUE), col=cols[1], main="", ylim=c(0,0.17), xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), lwd=2, las=1  )
for(i in 2:length(unique(fd2$period))) {
  lines(density(x=fd2$mean_aug_flow[fd2$period==i ], na.rm=TRUE), col=cols[i], lwd=2)
}
legend("topright", 
       inset=c(0, 0.1),
       legend=unique(fd1$period_name),
       col=cols,
       pch="l",
       bty="n" )
abline(v=xint_unscaled, col="gray", lty=4, lwd=2)

#hist(fd2$mean_aug_flow[fd2$period==1 ], col=adjustcolor( cols[1], alpha=0.5), breaks=8,  xlim=c(0,max(fd2$mean_aug_flow)))
#hist(fd2$mean_aug_flow[fd2$period==2 ], col=adjustcolor( cols[2], alpha=0.5), add=TRUE, breaks=8,  xlim=c(0,max(fd2$mean_aug_flow)))
#hist(fd2$mean_aug_flow[fd2$period==3 ], col=adjustcolor( cols[3], alpha=0.5), add= TRUE, breaks=8,  xlim=c(0,max(fd2$mean_aug_flow)))

#plot effect of mean aug flow on recruitment
#plot(log(d$wild_recruits/d$total_spawners) ~ d$aug_mean_flow_rear, xlab="Mean Aug flow cms (scaled)", ylab="log(R/S)")
par(mar=c(4,4,0.1,4))
plot(pred_flow_unscaled, pred_mean_mean, type="l", lwd=1.4,  ylim=c(min(pred_75_HPDI),  max(pred_25_HPDI)), xlim=c(0,max(d_unscaled$aug_mean_flow_rear)), xlab=expression("Mean Aug flow in Nicola River (m"^3*"s"^-1*")"), ylab=expression("log"[e]*"(Recruits/Spawener)"), las=1)
abline(h=0, lty=3)
#plot(log(d$wild_recruits/d$total_spawners) ~ d$aug_mean_flow_rear, xlab="Mean Aug flow cms", ylab="log(R/S)")
#   for(j in 4400:4500) {
#    curve(log(post$alpha[j]) - post$beta[j] * mean(d$total_spawners) + post$b5[j] * x, 
#           add=TRUE, lwd=2, col=adjustcolor("grey", 0.1))
#  }
#curve(log(mean(post$alpha)) - mean(post$beta) * mean(d$total_spawners) + mean(post$b5) * x, add=TRUE)
rethinking::shade(pred_mean_HPDI, pred_flow_unscaled, col=adjustcolor(col="black", alpha=0.2) )
lines(pred_flow_unscaled, pred_25_mean, col="red", lwd=1.4)
rethinking::shade(pred_25_HPDI, pred_flow_unscaled, col=adjustcolor(col="pink", alpha=0.3))
lines(pred_flow_unscaled, pred_75_mean, col="dodgerblue", lwd=1.4)
rethinking::shade(pred_75_HPDI, pred_flow_unscaled, col=adjustcolor(col="dodgerblue", alpha=0.2))
#axis(3, at=seq(min(d$aug_mean_flow_rear), max(d$aug_mean_flow_rear), length.out=10), labels=round(seq(min(d_unscaled$aug_mean_flow_rear), max(d_unscaled$aug_mean_flow_rear), length.out=10),3), line=3)

legend("bottomright", 
       inset=c(0, 0),
       legend=c("75% quantile spawners", "Mean spawners", "25% quantile spawners"),
       col=c("dodgerblue", "black", "pink"),
       pch="l",
       bty="n" )

axis(side=4, labels=sec_yax, at=log(sec_yax), las=1)
mtext("Recruits/Spawner", side=4, line=2, cex=0.7)
abline(v=xint_unscaled, col="gray", lty=4, lwd=2)

dev.off()


# Changes in carrying capacity, etc. over hydrometric flow periods------
# See Hilborn and Walters 2013 , Table 7.2 for equations 
# calculate productivity using flow from different periods - note that other environmental parameters can be dropped since mean values are 0 since they are scaled. 
#prod <- post$alpha*exp(post$b5 * BOOTSTRAP FLOW VALS HERE)
# calculate carrying capacity
CC <- log(post$alpha)/post$beta
# calculate SMSY using posterior samples
SMSY <- log(post$alpha)/post$beta * (0.5 - 0.07*log(post$alpha))




# Plot data with model, using last 100 posterior samples and each 26 lines of observed data -------
# png(filename = "./figures/fig_R~S_with_model_runs_lognormal.png", width=1000, height=800, pointsize = 25)
# par(mar=c(4,4,0,0) +0.1)
# plot(d$wild_recruits ~ d$total_spawners, xlab="Spawners", ylab="Recruits")
# for(i in 1:nrow(d)) {
#   for(j in 4400:4500) {
#     curve(post$alpha[j] * x * exp(-post$beta[j] * x + 
#                                      post$b1[j] * d$ocean_surv_anomaly[i] +
#                                      post$b2[j] * d$max_flow_fall_c[i] + 
#                                      post$b3[j] * d$mean_flow_aug_c[i]), 
#           add=TRUE, lwd=2, col=adjustcolor("grey", 0.1))
#   }
# }
# points(d$wild_recruits ~ d$total_spawners)
# curve(mean(post$alpha) * x * exp(-mean(post$beta) * x + 
#                                     mean(post$b1) * mean(d$ocean_surv_anomaly) +
#                                     mean(post$b2) * mean(d$max_flow_fall_c) + 
#                                     mean(post$b3) * mean(d$mean_flow_aug_c)), col="dodger blue", lwd=3, add=TRUE)
# dev.off()

# Circle graph of change in flows --------
# setup for circle graph of 
table(fd$year)
table(fd$period)
str(fd) 
unique(fd$Parameter)
fd$yday <- yday(fd$Date)
fd_avg_prd <- fd %>% group_by(period, yday) %>% summarise(avg_flow = mean(Value, na.rm=TRUE))
ggplot(fd_avg_prd, aes(y=avg_flow, x=yday, colour=factor(period))) + 
  geom_line(size=1.2) +
  scale_color_manual(values = cols) +
  theme_classic()
# long to wide format to get percent change between periods
fd_change <- fd_avg_prd %>% pivot_wider(names_from=period, names_prefix= "prd", values_from= avg_flow )
fd_change$dif4_2 <- (fd_change$prd4 - fd_change$prd2) / fd_change$prd2 * 100
fd_change$dif4_1 <- (fd_change$prd4 - fd_change$prd1) / fd_change$prd1 * 100
fd_change$dif4_3 <- (fd_change$prd4 - fd_change$prd3) / fd_change$prd3 * 100
# back to long format 
fd_change_l <- fd_change[,c(1,6:8)] %>% pivot_longer(cols=c(dif4_2, dif4_1, dif4_3), names_to = "prd_change", values_to = "perc_change")

# setup for labels
days_month <- as.vector(table(fd$month, fd$year)[,"1970"])
days_month 
month_breaks <- cumsum(days_month)-31 + 1
month_labels <- format(ISOdatetime(2000,1:12,1,0,0,0),"%b")

# days_month_water_yr <- days_month[c(9:12, 1:8)]
# month_breaks_water_yr <- cumsum(days_month_water_yr)-30+1
# month_labels_water_yr <- format(ISOdatetime(2000,c(9:12,1:8),1,0,0,0),"%b")

range(fd_change$dif4_2)
# circle
fig_hyd_change_periods <- fd_change_l[fd_change_l$prd_change %in% c("dif4_2"), ] %>% 
  ggplot(aes(y=perc_change, x=yday,  fill=perc_change)) + 
  geom_col() +
  geom_path() +
  #geom_area(aes( y=perc_change), fill="gray") +
  #geom_point(shape=21, size=2) +
  #geom_point(shape=16,size=1) +
  #scale_shape_manual(values=c(17,16)) +
  #scale_fill_gradient2(high="blue", mid="white", low="red", midpoint=0,  breaks=seq(-50,150,50), labels=as.character(seq(-50,150,50)), limits=c(-50,150), name="Percent change in\naverage daily flow,\n1957-1974 to 1992-2014") +
  scale_fill_gradientn(colours=c("red","orange", "white", "green", "blue"), values=c(0,0.12, scales::rescale(x=0, to=c(0,1), from=range(fd_change_l$perc_change[fd_change_l$prd_change =="dif4_2"])), 0.5, 1), breaks=seq(-50,150,50), labels=as.character(seq(-50,150,50)), limits=c(-50,150), name="Percent change in\naverage daily flow,\n1957-1974 to 1992-2014") +
  coord_polar() + 
  scale_y_continuous(limits=c(-200,max(fd_change_l$perc_change)), expand=c(0,0), breaks=NULL) +
  #scale_y_continuous(limits=c(-300,100), expand=c(0,0)) +
  scale_x_continuous(breaks=month_breaks, labels=month_labels, minor_breaks=NULL) +
  geom_hline(aes(yintercept=0)) +
  xlab("") +
  ylab("") +
  theme_classic() +
  theme(axis.line = element_blank())
fig_hyd_change_periods 
ggsave("./figures/fig_hyd_change_periods.png", fig_hyd_change_periods, width=10, height=6 )


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

