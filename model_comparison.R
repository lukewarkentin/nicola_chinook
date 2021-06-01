# Run all candidate models and then do WAIC and loo comparisons

library(dplyr)
library(ggplot2)
library(rstan)
library(rethinking)
#library(rstanarm)
library(loo)
rstan_options(auto_write=TRUE)
options(mc.cores = parallel::detectCores())


d <- read.csv("./data/model_data.csv")

# Optional: run without first year on record, which had very low smolt to age 3 survival
# d <- d[!d$brood_year==1992, ]
 
# Models to compare:


# Two beta models:
# 0: Base model (ocean survival only)
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
# 0b: Base model (ocean survival only)
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
# Two beta models:---------
# Model 0: Base model------
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
# Model 0: Base model------
inits_0b= rep(
  list(
    list(lnalpha = runif(1, 0,3), 
         beta = rnorm(1, 0.0002, 0.0001),
         b1 = rnorm(1, mean=0, sd=0.1),
         tau =  runif(1, 0, 2)
    )), 3
)
pars_track_0b <- c("alpha","beta","b1",
                  "tau", "pp_log_RS",  "log_lik")

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
# Two beta models
fit_ricker_0 <- stan( file = "stan/ricker_linear_logRS_0.stan", 
                      data=dat, chains=3, iter=100000, init=inits_0, pars=pars_track_0)
fit_ricker_1 <- stan( file = "stan/ricker_linear_logRS_1.stan", 
                         data=dat, chains=3, iter=100000, init=inits_1, pars=pars_track_1)
fit_ricker_2 <- stan( file = "stan/ricker_linear_logRS_2.stan", 
                         data=dat, chains=3, iter=100000, init=inits_2,  pars=pars_track_2)
fit_ricker_3 <- stan( file = "stan/ricker_linear_logRS_3.stan", 
                         data=dat, chains=3, iter=100000, init=inits_3, pars=pars_track_3)
fit_ricker_4 <- stan( file = "stan/ricker_linear_logRS_4.stan", 
                         data=dat, chains=3, iter=100000, init=inits_4, pars=pars_track_4)
fit_ricker_5 <- stan( file = "stan/ricker_linear_logRS_5.stan", 
                         data=dat, chains=3, iter=100000, init=inits_5,  pars=pars_track_5)
fit_ricker_6 <- stan( file = "stan/ricker_linear_logRS_6.stan", 
                         data=dat, chains=3, iter=100000, init=inits_6, pars=pars_track_6)
fit_ricker_7 <- stan( file = "stan/ricker_linear_logRS_7.stan", 
                      data=dat, chains=3, iter=100000, init=inits_7, pars=pars_track_7)
fit_ricker_8 <- stan( file = "stan/ricker_linear_logRS_8.stan", 
                      data=dat, chains=3, iter=100000, init=inits_8, pars=pars_track_8)
fit_ricker_9 <- stan( file = "stan/ricker_linear_logRS_9.stan", 
                      data=dat, chains=3, iter=100000, init=inits_9, pars=pars_track_9)
fit_ricker_10 <- stan( file = "stan/ricker_linear_logRS_10.stan", 
                      data=dat, chains=3, iter=100000, init=inits_10, pars=pars_track_10)
fit_ricker_11 <- stan( file = "stan/ricker_linear_logRS_11.stan", 
                       data=dat, chains=3, iter=100000, init=inits_11, pars=pars_track_11)
# One beta models
fit_ricker_0b <- stan( file = "stan/ricker_linear_logRS_0b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_0b, pars=pars_track_0b)
fit_ricker_1b <- stan( file = "stan/ricker_linear_logRS_1b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_1b, pars=pars_track_1b)
fit_ricker_2b <- stan( file = "stan/ricker_linear_logRS_2b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_2b, pars=pars_track_2b)
fit_ricker_3b <- stan( file = "stan/ricker_linear_logRS_3b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_3b, pars=pars_track_3b)
fit_ricker_4b <- stan( file = "stan/ricker_linear_logRS_4b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_4b, pars=pars_track_4b)
fit_ricker_5b <- stan( file = "stan/ricker_linear_logRS_5b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_5b, pars=pars_track_5b)
fit_ricker_6b <- stan( file = "stan/ricker_linear_logRS_6b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_6b, pars=pars_track_6b)
fit_ricker_7b <- stan( file = "stan/ricker_linear_logRS_7b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_7b, pars=pars_track_7b)
fit_ricker_8b <- stan( file = "stan/ricker_linear_logRS_8b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_8b, pars=pars_track_8b)
fit_ricker_9b <- stan( file = "stan/ricker_linear_logRS_9b.stan", 
                      data=dat, chains=3, iter=100000, init=inits_9b, pars=pars_track_9b)
fit_ricker_10b <- stan( file = "stan/ricker_linear_logRS_10b.stan", 
                       data=dat, chains=3, iter=100000, init=inits_10b, pars=pars_track_10b)
fit_ricker_11b <- stan( file = "stan/ricker_linear_logRS_11b.stan", 
                       data=dat, chains=3, iter=100000, init=inits_11b, pars=pars_track_11b)

# Compare models ---------- 
# Output is a table with WAIC, LOO and R2 values.
# Optional graphs for each of these comaprisons
#fits <- ls(pattern="fit_ricker")

waic_tab <- rethinking::compare(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, 
                                fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11, 
                                fit_ricker_0b, fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b,
                                fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b)
# Plot WAIC fig
png("./figures/fig_WAIC_model_compare.png", pointsize=20, width=500, height=1000)
plot(waic_tab, SE=TRUE, dSE=TRUE)
dev.off()

# Save WAIC table
waic <- waic_tab
# fix row names
row.names(waic) <- sub("fit_ricker_", "Model ", row.names(waic))
#waic$`Cumulative weight` <- cumsum(waic$weight) # add cumulative weight column
# save WAIC table
write.csv(round(waic,2), "WAIC_table.csv")

# Leave one out 
models <- list(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, 
              fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11, 
              fit_ricker_0b, fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b,
              fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b)
models
# Do leave-one-out cross validation 
loovals <- lapply(models, loo, cores = parallel::detectCores())
loovals[[1]]
plot(loovals[[8]]) # plot Pareto shape k value for each observation
loo_mod_compare <- loo_compare(loovals)
lootab <- as.data.frame(loo_mod_compare)
#row.names(lootab) <- paste0("fit_ricker_", as.numeric(substr(row.names(lootab),6,8))-1) 
row.names(lootab) <- as.numeric(substr(row.names(lootab),6,8))-1
row.names(lootab) <- ifelse(as.numeric(row.names(lootab)) <= 11, row.names(lootab), paste0(as.numeric(row.names(lootab))-12, "b"))
row.names(lootab) <- paste0("Model ", row.names(lootab))

# save loo table
write.csv(round(lootab,2), "./loo_comparison.csv")

# graph
png("./figures/fig_LOO_model_compare.png", pointsize=20, width=500, height=1000)
par(mar=c(4,6,0,0)+0.1)
plot(x=lootab$elpd_loo, y=rev(1:nrow(lootab)), yaxt='n', ylab="", xlab="ELPD LOO", xlim=c(min(lootab$elpd_loo - lootab$se_elpd_loo), max(lootab$elpd_loo + lootab$se_elpd_loo)))
axis(2, at=seq_len(nrow(lootab)), labels = rev(rownames(lootab)), las=2)
segments(x0=lootab$elpd_loo - lootab$se_elpd_loo, x1=lootab$elpd_loo + lootab$se_elpd_loo, y0=rev(1:nrow(lootab)))
abline(v=max(lootab$elpd_loo))
points(x=max(lootab$elpd_loo) + lootab$elpd_diff, y=rev(1:nrow(lootab))+0.5, pch=2, col="gray")
segments(x0=lootab$elpd_loo - lootab$se_diff, x1=lootab$elpd_loo + lootab$se_diff, y0=rev(1:nrow(lootab)) +0.5, col="gray")
dev.off()

# Do Bayesian model stacking to get model average
# https://mc-stan.org/loo/articles/loo2-weights.html
# https://mc-stan.org/loo/reference/loo_model_weights.html

loo_model_weights(loovals, method="stacking")

# Check R2 of the models
# get posterior estimates for log(R/S)
post_list <- lapply(X=models, FUN=as.data.frame) # make list of posterior data frames
ppd_list <- lapply(post_list, FUN=function(i) { i[, grep("pp_log_RS", colnames(i))] }) # select only posteriors for predicted log(R/S)
mn_pp_log_RS <- lapply(ppd_list, function(i) { apply(i, 2, mean)}) # get means of predicted log(R/S)
preds <- as.data.frame(t(do.call(rbind, mn_pp_log_RS))) # bind all means for each observation into one data frame
mods <- paste0("Model ", c(0:11, paste0(0:11, "b"))) # make a vector of model names
names(preds) <- mods # name predictions after models
obs_log_RS <- log(d$wild_recruits / d$ total_spawners) # get observed log(R/S)
rsq <- apply(preds, 2, function(i) {cor(obs_log_RS,i)^2}) # get r-squared for each model
rsqs <-sort(rsq) # sort largest to smallest
r_df <- as.data.frame(rsq, row.names = NULL) # make into data frame
r_df$model <- row.names(r_df)

# Plot R-squared values
png("./figures/fig_Rsquared_model_compare.png", pointsize=20, width=800, height=800)
par(mar=c(4,6,0,0)+0.1)
plot(x=rsqs, y= 1:length(rsqs), xlab="R-squared", yaxt="n", ylab="",  type="p")
axis(2, at=seq_len(length(rsqs)), labels = names(rsqs), las=2)
text(x=rsqs, y= 1:length(rsqs)+0.4, label=round(rsqs,2), cex=0.7)
dev.off()


# Combine WAIC, LOO and R2 tables to print
comp <- merge(waic, lootab, by="row.names")
comp1 <- merge(comp, r_df, by.x="Row.names", by.y="model")
comp1[,-1] <- signif(comp1[,-1], 3) # round all numeric values to 3 significant figures
write.csv(comp1[order(comp1$WAIC),], "data_out/model_comparison_master.csv")


# Compare parameter estimates - environmental variable effect estimates
pars_plot <- c("b1", "b2", "b3", "b4", "b5")
png("./figures/fig_parameter_estimates_model_compare.png", width=300, height=1300, pointsize = 12)
coeftab_plot(coeftab(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, 
             fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11, 
             fit_ricker_0b, fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b,
             fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b), 
             pars=pars_plot)
dev.off()

# beta terms - something not right here - coeftab doesn't match traceplot
pars_plot_2 <- c("betaW", "betaH", "beta")
png("./figures/fig_beta_estimates_model_compare.png", width=300, height=800, pointsize = 12)
coeftab_plot(coeftab(fit_ricker_0, fit_ricker_1, fit_ricker_2, fit_ricker_3, fit_ricker_4, fit_ricker_5, 
             fit_ricker_6, fit_ricker_7, fit_ricker_8, fit_ricker_9, fit_ricker_10, fit_ricker_11, 
             fit_ricker_0b, fit_ricker_1b, fit_ricker_2b, fit_ricker_3b, fit_ricker_4b, fit_ricker_5b,
             fit_ricker_6b, fit_ricker_7b, fit_ricker_8b, fit_ricker_9b, fit_ricker_10b, fit_ricker_11b, se=TRUE, digits=10), pars=pars_plot_2)
dev.off()

# Get coefficient estimates for different models
mod_sum_8 <- round(summary(fit_ricker_8, probs=c(0.1,0.9))$summary,6)
mod_sum_8
write.csv(mod_sum_8, "estimates_model8.csv")
# Get carring capacity for hatchery vs. wild fish
cc_w <- log(mod_sum_8[1,1])/mod_sum_8[2,1]
cc_h <- log(mod_sum_8[1,1])/mod_sum_8[3,1]
cc_h / cc_w
cc_w * 0.542




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