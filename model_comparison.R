
library(dplyr)
library(ggplot2)
library(rstan)
library(rethinking)
rstan_options(auto_write=TRUE)

rm(list=ls())

d <- read.csv("./data/model_data.csv")

# Models to compare:
# Base model (ocean survival only)
# 1: Base + spawning flows
# 2: Base + fall flood
# 3: Base + ice days
# 4: Base + rearing flows
# 5: Base + summer terms (spawning and rearing flows)
# 6: Base + fall/winter terms (fall flood and ice days)
# 7: Full - spawning flows
# 8: Full - fall flood
# 9: Full - ice days
#10: Full - rearing flows
# Full model


# Declare data --------
dat <- list(
  N = nrow(d),
  log_RS = log(d$wild_recruits/d$total_spawners),
  S = d$total_spawners, 
  ocean_surv = d$smolt_age3_survival,
  aug_mean_flow = d$aug_mean_flow,
  sep_dec_max_flow = d$sep_dec_max_flow,
  aug_mean_flow_rear = d$aug_mean_flow_rear,
  ice_days = d$ice_days
)

# Declare initial values and parameters to track for each model --------
# Base model------
inits_base= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_base <- c("alpha", "beta","b1",
                     "tau", "pp_log_RS", "pp_R", "log_lik")

# Model 1---------
inits_1= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_1 <- c("alpha", "beta","b1",
                     "b2",
                     "tau", "pp_log_RS", "pp_R", "log_lik")
# Model 2----------
inits_2= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_2 <- c("alpha", "beta","b1",
                     "b3", 
                     "tau", "pp_log_RS", "pp_R", "log_lik")
# Model 3-----------
inits_3= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_3 <- c("alpha", "beta","b1",
                     "b4", 
                     "tau", "pp_log_RS", "pp_R", "log_lik")
# Model 4-------
inits_4 = rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_4 <- c("alpha", "beta","b1",
                     "b5",
                     "tau", "pp_log_RS", "pp_R", "log_lik")
# Model 5-------
inits_5= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b2 = rnorm(1, mean=0, sd=0.1),
         b5 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_5 <- c("alpha", "beta","b1",
                     "b2", 
                     "b5",
                     "tau", "pp_log_RS", "pp_R", "log_lik")
# Model 6 --------
inits_6= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         b3 = rnorm(1, mean=0, sd=0.1),
         b4 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_6 <- c("alpha", "beta","b1",
                      "b3", 
                      "b4",
                      "tau", "pp_log_RS", "pp_R", "log_lik")
# Model 7 ---------
inits_7= rep(
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
pars_track_7 <- c("alpha", "beta","b1",
                     "b3", 
                     "b4", 
                     "b5",
                     "tau", "pp_log_RS", "pp_R", "log_lik")

# Model 8---------
inits_8= rep(
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
pars_track_8 <- c("alpha", "beta","b1",
                     "b2", 
                     "b4", 
                     "b5",
                     "tau", "pp_log_RS", "pp_R", "log_lik")

# Model 9---------
inits_9= rep(
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
pars_track_9 <- c("alpha", "beta","b1",
                     "b2", 
                     "b3", 
                     "b5",
                     "tau", "pp_log_RS", "pp_R", "log_lik")

# Model 10---------
inits_10= rep(
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
pars_track_10 <- c("alpha", "beta","b1",
                     "b2", 
                     "b3", 
                     "b4",
                     "tau", "pp_log_RS", "pp_R", "log_lik")
# Full Model----------
inits_full= rep(
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
pars_track_full <- c("alpha", "beta","b1",
                     "b2", 
                     "b3", 
                     "b4", 
                     "b5",
                     "tau", "pp_log_RS", "pp_R", "log_lik")

# Fit models--------
fit_ricker_base <- stan( file = "ricker_linear_logRS_base.stan", 
                         data=dat, chains=3, iter=10000, init=inits_base, 
                         cores=2, pars=pars_track_base)
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
fit_ricker_full <- stan( file = "ricker_linear_logRS_full.stan", 
                         data=dat, chains=3, iter=10000, init=inits_full, 
                         cores=2, pars=pars_track_full)
# Compare models ----------
#fits <- ls(pattern="fit_ricker")

waic_tab <- compare(fit_ricker_base, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_full)
png("./figures/fig_WAIC_model_compare.png", pointsize=20, width=500)
plot(waic_tab, SE=TRUE, dSE=TRUE)
dev.off()
waic <- waic_tab@output
waic$cumulative_weight <- cumsum(waic$weight)
waic
write.csv(waic, "WAIC_table.csv")

# Compare parameter estimates
pars_plot <- c("b1","b2", "b3", "b4","b5")
png("./figures/fig_parameter_estimates_model_compare.png", width=800, height=2000, pointsize = 18)
plot(coeftab(fit_ricker_base, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_full), pars=pars_plot)
dev.off()

# Check beta only
plot(coeftab(fit_ricker_base, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_full), pars="beta")

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

