# Run all candidate models and then do WAIC and loo comparisons


library(dplyr)
library(ggplot2)
library(rstan)
library(rethinking)
library(rstanarm)
library(loo)
rstan_options(auto_write=TRUE)

rm(list=ls())

d <- read.csv("./data/model_data.csv")

# Models to compare:

# 0: Base model (ocean survival only)

# Two beta models:
# 1: Base + spawning flows
# 2: Base + fall flood
# 3: Base + ice days
# 4: Base + rearing flows
# 5: Base + summer terms (spawning and rearing flows)
# 6: Base + fall/winter terms (fall flood and ice days)
# 7: Full - spawning flows
# 8: Full - fall flood
# 9: Full - ice days
# 10: Full - rearing flows
# 11: Full model with one beta

# One beta models:
# 1b: Base + spawning flows
# 2b: Base + fall flood
# 3b: Base + ice days
# 4b: Base + rearing flows
# 5b: Base + summer terms (spawning and rearing flows)
# 6b: Base + fall/winter terms (fall flood and ice days)
# 7b: Full - spawning flows
# 8b: Full - fall flood
# 9b: Full - ice days
# 10b: Full - rearing flows
# 11b: Full model with two betas



# Declare data --------
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

# Declare initial values and parameters to track for each model --------
# Base model------
inits_0= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         betaH = rnorm(1, 0.0002, 0.0001),
         betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_0 <- c("alpha","betaW", "betaH","b1",
                     "tau", "pp_log_RS",  "log_lik")

# Two beta models:---------
# Model 1---------
inits_1= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_1 <- c("alpha","betaW", "betaH","b1",
                     "b2",
                     "tau", "pp_log_RS",  "log_lik")
# Model 2----------
inits_2= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_2 <- c("alpha","betaW", "betaH","b1",
                     "b3", 
                     "tau", "pp_log_RS",  "log_lik")
# Model 3-----------
inits_3= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_3 <- c("alpha","betaW", "betaH","b1",
                     "b4", 
                     "tau", "pp_log_RS",  "log_lik")
# Model 4-------
inits_4 = rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_4 <- c("alpha","betaW", "betaH","b1",
                     "b5",
                     "tau", "pp_log_RS",  "log_lik")
# Model 5-------
inits_5= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_5 <- c("alpha","betaW", "betaH","b1",
                     "b2", 
                     "b5",
                     "tau", "pp_log_RS",  "log_lik")
# Model 6 --------
inits_6= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_6 <- c("alpha","betaW", "betaH","b1",
                      "b3", 
                      "b4",
                      "tau", "pp_log_RS",  "log_lik")
# Model 7 ---------
inits_7= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_7 <- c("alpha","betaW", "betaH","b1",
                     "b3", 
                     "b4", 
                     "b5",
                     "tau", "pp_log_RS",  "log_lik")

# Model 8---------
inits_8= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
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

# Model 9---------
inits_9= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_9 <- c("alpha","betaW", "betaH","b1",
                     "b2", 
                     "b3", 
                     "b5",
                     "tau", "pp_log_RS",  "log_lik")

# Model 10---------
inits_10= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_10 <- c("alpha","betaW", "betaH","b1",
                     "b2", 
                     "b3", 
                     "b4",
                     "tau", "pp_log_RS",  "log_lik")

# Model 11: Full Model, two betas ----------
inits_11= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
                   betaH = rnorm(1, 0.0002, 0.0001),          betaW = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_11 <- c("alpha","betaW", "betaH","b1",
                     "b2", 
                     "b3", 
                     "b4", 
                     "b5",
                     "tau", "pp_log_RS",  "log_lik")

# One beta models--------
# Model 1b---------
inits_1b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_1b <- c("alpha","beta","b1",
                  "b2",
                  "tau", "pp_log_RS",  "log_lik")
# Model 2b----------
inits_2b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_2b <- c("alpha","beta","b1",
                  "b3", 
                  "tau", "pp_log_RS",  "log_lik")
# Model 3b-----------
inits_3b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_3b <- c("alpha","beta","b1",
                  "b4", 
                  "tau", "pp_log_RS",  "log_lik")
# Model 4b-------
inits_4b = rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_4b <- c("alpha","beta","b1",
                  "b5",
                  "tau", "pp_log_RS",  "log_lik")
# Model 5b-------
inits_5b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_5b <- c("alpha","beta","b1",
                  "b2", 
                  "b5",
                  "tau", "pp_log_RS",  "log_lik")
# Model 6b --------
inits_6b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_6b <- c("alpha","beta","b1",
                  "b3", 
                  "b4",
                  "tau", "pp_log_RS",  "log_lik")
# Model 7b ---------
inits_7b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_7b <- c("alpha","beta","b1",
                  "b3", 
                  "b4", 
                  "b5",
                  "tau", "pp_log_RS",  "log_lik")

# Model 8b---------
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

# Model 9b---------
inits_9b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_9b <- c("alpha","beta","b1",
                  "b2", 
                  "b3", 
                  "b5",
                  "tau", "pp_log_RS",  "log_lik")

# Model 10b---------
inits_10b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_10b <- c("alpha","beta","b1",
                   "b2", 
                   "b3", 
                   "b4",
                   "tau", "pp_log_RS",  "log_lik")

# 11b: Full Model, one beta----------
inits_11b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_11b <- c("alpha", "beta","b1",
                   "b2", 
                   "b3", 
                   "b4", 
                   "b5",
                   "tau", "pp_log_RS",  "log_lik")


# Fit models--------
fit_ricker_0 <- stan( file = "ricker_linear_logRS_0_base.stan", 
                         data=dat, chains=3, iter=10000, init=inits_0, 
                         cores=2, pars=pars_track_0)
# Two beta models
fit_ricker_1 <- stan( file = "ricker_linear_logRS_1.stan", 
                         data=dat, chains=3, iter=10000, init=inits_1, 
                         cores=2, pars=pars_track_1)
fit_ricker_2 <- stan( file = "ricker_linear_logRS_2.stan", 
                         data=dat, chains=3, iter=10000, init=inits_2, 
                         cores=2, pars=pars_track_2)
fit_ricker_3 <- stan( file = "ricker_linear_logRS_3.stan", 
                         data=dat, chains=3, iter=10000, init=inits_3, 
                         cores=2, pars=pars_track_3)
fit_ricker_4 <- stan( file = "ricker_linear_logRS_4.stan", 
                         data=dat, chains=3, iter=10000, init=inits_4, 
                         cores=2, pars=pars_track_4)
fit_ricker_5 <- stan( file = "ricker_linear_logRS_5.stan", 
                         data=dat, chains=3, iter=10000, init=inits_5, 
                         cores=2, pars=pars_track_5)
fit_ricker_6 <- stan( file = "ricker_linear_logRS_6.stan", 
                         data=dat, chains=3, iter=10000, init=inits_6, 
                         cores=2, pars=pars_track_6)
fit_ricker_7 <- stan( file = "ricker_linear_logRS_7.stan", 
                      data=dat, chains=3, iter=10000, init=inits_7, 
                      cores=2, pars=pars_track_7)
fit_ricker_8 <- stan( file = "ricker_linear_logRS_8.stan", 
                      data=dat, chains=3, iter=10000, init=inits_8, 
                      cores=2, pars=pars_track_8)
fit_ricker_9 <- stan( file = "ricker_linear_logRS_9.stan", 
                      data=dat, chains=3, iter=10000, init=inits_9, 
                      cores=2, pars=pars_track_9)
fit_ricker_10 <- stan( file = "ricker_linear_logRS_10.stan", 
                      data=dat, chains=3, iter=10000, init=inits_10, 
                      cores=2, pars=pars_track_10)
fit_ricker_11 <- stan( file = "ricker_linear_logRS_11.stan", 
                       data=dat, chains=3, iter=10000, init=inits_11, 
                       cores=2, pars=pars_track_11)
# One beta models
fit_ricker_1b <- stan( file = "ricker_linear_logRS_1b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_1b, 
                      cores=2, pars=pars_track_1b)
fit_ricker_2b <- stan( file = "ricker_linear_logRS_2b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_2b, 
                      cores=2, pars=pars_track_2b)
fit_ricker_3b <- stan( file = "ricker_linear_logRS_3b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_3b, 
                      cores=2, pars=pars_track_3b)
fit_ricker_4b <- stan( file = "ricker_linear_logRS_4b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_4b, 
                      cores=2, pars=pars_track_4b)
fit_ricker_5b <- stan( file = "ricker_linear_logRS_5b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_5b, 
                      cores=2, pars=pars_track_5b)
fit_ricker_6b <- stan( file = "ricker_linear_logRS_6b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_6b, 
                      cores=2, pars=pars_track_6b)
fit_ricker_7b <- stan( file = "ricker_linear_logRS_7b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_7b, 
                      cores=2, pars=pars_track_7b)
fit_ricker_8b <- stan( file = "ricker_linear_logRS_8b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_8b, 
                      cores=2, pars=pars_track_8b)
fit_ricker_9b <- stan( file = "ricker_linear_logRS_9b.stan", 
                      data=dat, chains=3, iter=10000, init=inits_9b, 
                      cores=2, pars=pars_track_9b)
fit_ricker_10b <- stan( file = "ricker_linear_logRS_10b.stan", 
                       data=dat, chains=3, iter=10000, init=inits_10b, 
                       cores=2, pars=pars_track_10b)
fit_ricker_11b <- stan( file = "ricker_linear_logRS_11b.stan", 
                       data=dat, chains=3, iter=10000, init=inits_11b, 
                       cores=2, pars=pars_track_11b)

# Compare models ---------- 
#fits <- ls(pattern="fit_ricker")

waic_tab <- rethinking::compare(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11,
                                fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b, fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b  )
png("./figures/fig_WAIC_model_compare.png", pointsize=20, width=500)
plot(waic_tab, SE=TRUE, dSE=TRUE)
dev.off()
waic <- waic_tab@output
waic$cumulative_weight <- cumsum(waic$weight)
waic
write.csv(waic, "WAIC_table.csv")

# Compare parameter estimates - environmental variable effect estimates
pars_plot <- c("b1","b2", "b3", "b4","b5")
png("./figures/fig_parameter_estimates_model_compare.png", width=800, height=3000, pointsize = 18)
plot(coeftab(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11,
             fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b, fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b), pars=pars_plot)
dev.off()

# beta terms - something not right here - coeftab doesn't match traceplot
pars_plot_2 <- c("betaW", "betaH", "beta")
png("./figures/fig_beta_estimates_model_compare.png", width=800, height=2000, pointsize = 18)
plot(coeftab(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11,
             fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b, fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b), pars=pars_plot_2)
dev.off()

# Leave one out 
models <-list(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11,
              fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b, fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b)
loovals <- lapply(models, loo)
plot(loovals[[8]]) # plot Pareto shape k value for each observation
loo_mod_compare <- loo_compare(loovals)
lootab <- as.data.frame(loo_mod_compare)
#row.names(lootab) <- paste0("fit_ricker_", as.numeric(substr(row.names(lootab),6,8))-1) 
row.names(lootab) <- as.numeric(substr(row.names(lootab),6,8))-1

row.names(lootab) <- ifelse(as.numeric(row.names(lootab)) <= 11, row.names(lootab), paste0(as.numeric(row.names(lootab))-11, "b"))

# save loo table
write.csv(lootab, "./loo_comparison.csv")

# graph
str(lootab)
png("./figures/fig_LOO_model_compare.png", pointsize=20, width=500)
par(mar=c(4,6,0,0)+0.1)
plot(x=lootab$elpd_loo, y=1:nrow(lootab), yaxt='n', ylab="", xlab="ELPD LOO", xlim=c(-33, -27))
axis(2, at=seq_len(nrow(lootab)), labels = rownames(lootab), las=2)
segments(x0=lootab$elpd_loo - lootab$se_elpd_loo, x1=lootab$elpd_loo + lootab$se_elpd_loo, y0=1:nrow(lootab))
abline(v=max(lootab$elpd_loo))
points(x=max(lootab$elpd_loo) + lootab$elpd_diff, y=1:nrow(lootab)+0.5, pch=2, col="gray")
segments(x0=lootab$elpd_loo - lootab$se_diff, x1=lootab$elpd_loo + lootab$se_diff, y0=1:nrow(lootab) +0.5, col="gray")
dev.off()


#plot(x=1:18, y=1:18, pch=1:18)

# result is that 

# OBSOLETE: AUTOCORRELATION
# inits_AR= rep(
#   list(
#     list(#alpha=rnorm(1, mean= 3, sd= 1), #for non-linear
#       lnalpha = runif(1, 0,3), # for linear
#       beta = rnorm(1, 0.0002, 0.0001),
#       b1 = rnorm(1, mean=0, sd=0.1),
#       b2 = rnorm(1, mean=0, sd=0.1),
#       b3 = rnorm(1, mean=0, sd=0.1),
#       b4 = rnorm(1, mean=0, sd=0.1),
#       b5 = rnorm(1, mean=0, sd=0.1),
#       tau =  runif(1, 0, 2),
#       phi = runif(1, -0.99, 0.99),
#       log_resid0 = rnorm(1, mean=0, sd = rgamma(1, shape=0.01, scale=0.01) * (1- runif(1, -0.99, 0.99)^2))
#     )), 3
# )
# pars_track_AR <- c("alpha", "beta","b1",
#                    "b2", 
#                    "b3", 
#                    "b4", 
#                    "b5", 
#                    #"b6", 
#                    "tau", "phi", "log_resid0", "log_resid", "tau_red", "pp_log_RS", "pp_R",
#                    "log_lik")

# # Fit stan model
# fit_ricker_AR <- stan( file = "ricker_linear_logRS_autocor.stan", 
#                       data=dat, chains=3, iter=10000, init=inits_AR, 
#                       cores=2, pars=pars_track_AR)