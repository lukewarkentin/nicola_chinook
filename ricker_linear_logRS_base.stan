data {
  int N; // number rows
  vector[N] log_RS; // log of wild recruits per spawner
  vector[N] S; // total spawners
  vector[N] ocean_surv; // smolt to age 3 survival
  }

parameters {
  real lnalpha; // log(alpha) term
  real beta; // beta for total spawners
  real b1; // effect of ocean survival 
  real tau; // error term
  }

transformed parameters {
  vector[N] pred_log_RS; // predicted log(recruits/spawner)
  real alpha; // productivity
  pred_log_RS = lnalpha - beta * S + b1 * ocean_surv; // model predicting recruits from alpha and beta and covariates
  alpha = exp(lnalpha); // exponentiate alpha
  }

model {
  log_RS ~ normal(pred_log_RS, tau); // likelihood function for linear model
  lnalpha ~ uniform(0,3); // prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
  beta ~ uniform(0,10); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  b1 ~ normal(0, 1); // prior for effect of ocean survival
  tau ~ gamma(0.01, 0.01); // prior error
  }

generated quantities {
  vector[N] pp_log_RS;
  vector[N] pp_R;
  vector[N] log_lik; 
  for (i in 1:N) {
    pp_log_RS[i] = normal_rng(pred_log_RS[i], tau); // generate predicted log(recruits/spawner)
    pp_R[i] = exp(pp_log_RS[i])*S[i]; // exponentiate predicted log(recruits/spawner) and multiply by spawners to get untransformed predicted recruits
    log_lik[i] = normal_lpdf(log_RS[i] | pred_log_RS[i], tau); // generate log likelihoods
    }
  }
