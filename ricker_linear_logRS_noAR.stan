// see https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html 
data {
  int N; // number rows
  vector[N] log_RS; // log of wild recruits per spawner
  vector[N] S; // total spawners
  vector[N] ocean_surv; // ocean survival anomaly of hatchery fish
  vector[N] aug_mean_flow; // mean august flow in spawning year
  vector[N] sep_dec_max_flow; // max flow sep-dec in spawning year
  vector[N] ice_days;// 
  vector[N] aug_mean_flow_rear; // mean aug flow in rearing year
  //vector[N] jan_feb_max_flow_rear; // 
  }

parameters {
  real lnalpha; // alpha term
  real beta; // beta for total spawners
  real b1; // effect of ocean survival 
  real b2; // effect of aug_mean_flow - spawning
  real b3; // effect of sep-dec max flow - spawning/incubation
  real b4; // effect of ice_days - incubation
  real b5; // effect of mean aug flow during rearing - rearing
  //real b6; // effect of jan_feb_max_flow_rear - rearing
  real tau; // white noise process error - see Atnarko code, by Brennan Connors (BC), BC***
  }

transformed parameters {
  vector[N] pred_log_RS; // predicted log (recruits/spawner) 
  real alpha; // productivity
  pred_log_RS = lnalpha - beta * S +  b1 * ocean_surv + b2 * aug_mean_flow + b3 * sep_dec_max_flow
  + b4 * ice_days + b5 * aug_mean_flow_rear ; // model predicting recruits from alpha and beta and covariates
  alpha = exp(lnalpha); // exponentiate alpha
  }

model {
  log_RS ~ normal(pred_log_RS, tau); // likelihood function for linear model
  lnalpha ~ uniform(0,3); // prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
  beta ~ uniform(0,10); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  b1 ~ normal(0, 1); // prior for slope of effect of ocean survival anomaly
  b2 ~ normal(0, 1); // 
  b3 ~ normal(0, 1); // 
  b4 ~ normal(0, 1); //
  b5 ~ normal(0, 1); //
  //b6 ~ normal(0, 1); //
  tau ~ gamma(0.01, 0.01); // prior for white noise process error, BC***
  }

generated quantities {
  vector[N] pp_log_RS;
  vector[N] pp_R;
  //vector[N] log_lik; 
  for (i in 1:N) {
    pp_log_RS[i] = normal_rng(pred_log_RS[i], tau); // generate predicted log(recruits/spawner)
    pp_R[i] = exp(pp_log_RS[i])*S[i]; // exponentiate predicted log(recruits/spawner) and multiply by spawners to get untransformed predicted recruits
    //log_lik[i] = normal_lpdf(log_RS[i] | pred_log_RS[i], tau); // generate log likelihoods
    }
  }
