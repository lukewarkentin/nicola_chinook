// see https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html 
data {
  int N; // number rows
  vector[N] log_R; // log recruits
  vector[N] SW; // wild spawners
  vector[N] SH; // hatchery spawners
  vector[N] ocean_surv; // ocean survival anomaly of hatchery fish
  //vector[N] jul_mean_flow;  REMOVED MONTHS WITH CORRELATIONS >0.60
  vector[N] aug_mean_flow;
  //vector[N] sep_mean_flow;
  vector[N] oct_mean_flow;
  vector[N] nov_mean_flow;
  vector[N] dec_mean_flow;
  //vector[N] jan_mean_flow_rear;
  //vector[N] feb_mean_flow_rear;
  //vector[N] mar_mean_flow_rear;
  //vector[N] apr_mean_flow_rear; 
  vector[N] may_mean_flow_rear;
  //vector[N] jun_mean_flow_rear;
  //vector[N] jul_mean_flow_rear;
  vector[N] aug_mean_flow_rear;
  //vector[N] sep_mean_flow_rear;
  vector[N] oct_mean_flow_rear;
  vector[N] nov_mean_flow_rear;
  vector[N] dec_mean_flow_rear;
  }

parameters {
  real lnalpha; // alpha term
  real betaW; // beta term for wild spawners
  real betaH; // beta term for hatchery spawners
  real b1; // effect of ocean survival 
  //real b2; // jul_mean_flow
  real b3; // aug_mean_flow
  //real b4; // sep_mean_flow
  real b5; // oct_mean_flow
  real b6; // nov_mean_flow
  real b7; // dec_mean_flow
  //real b8; // jan_mean_flow_rear
  //real b9; // feb_mean_flow_rear
  //real b10; // mar_mean_flow_rear
  //real b11; // apr_mean_flow_rear
  real b12; // may_mean_flow_rear
  //real b13; // jun_mean_flow_rear
  //real b14; // jul_mean_flow_rear
  real b15; // aug_mean_flow_rear
  //real b16; // sep_mean_flow_rear
  real b17; // oct_mean_flow_rear
  real b18; // nov_mean_flow_rear
  real b19; // dec_mean_flow_rear
  real tau; // white noise process error - see Atnarko code, by Brennan Connors (BC), BC***
  real phi; // autocorrelation coeffiecient, BC***
  real log_resid0; // starting residual for autocorrelation, BC***
  }

transformed parameters {
  vector[N] log_R_mean1; // predicted mean recruits - this needs to be zero-bounded; either log-normal or negative binomial
  vector[N] log_R_mean2; // predicted mean recruits with autocorrelation
  vector[N] log_resid; // residuals vector
  real alpha; // productivity
  real tau_red; // standard deviation for prior for initial residual, BC***
  
  log_R_mean1 = lnalpha + log(SW+SH) - betaW * SW - betaH * SH +  b1 * ocean_surv +  b3* aug_mean_flow + b5* oct_mean_flow + b6* nov_mean_flow + b7* dec_mean_flow  +  b12* may_mean_flow_rear + b15* aug_mean_flow_rear  + b17* oct_mean_flow_rear + b18* nov_mean_flow_rear + b19* dec_mean_flow_rear; // model predicting recruits from alpha and beta and covariates
   for(i in 1:N) {
    log_resid[i] = log_R[i] - log_R_mean1[i]; // residual for autocorrelation
    }
  log_R_mean2[1] = log_R_mean1[1] + phi * log_resid0; // autocorrelation time step 1, BC***
  for(i in 2:N) { // autocorrelation, all other time steps, BC***
    log_R_mean2[i] = log_R_mean1[i] + phi * log_resid[i-1] ; 
    } 

  tau_red = tau * (1 - phi * phi); // BC***
  alpha = exp(lnalpha); // exponentiate alpha

  }

model {
  log_R ~ normal(log_R_mean2, tau); // likelihood function for linear model
  lnalpha ~ uniform(0,3); // prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
  betaW ~ uniform(0,10); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  betaH ~ uniform(0,10); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  b1 ~ normal(0, 1); // prior for slope of effect of ocean survival anomaly
  //b2 ~ normal(0, 1); // 
  b3 ~ normal(0, 1); // 
  //b4 ~ normal(0, 1); //
  b5 ~ normal(0, 1); //
  b6 ~ normal(0, 1); //
  b7 ~ normal(0, 1); //
  //b8 ~ normal(0, 1); //
  //b9 ~ normal(0, 1); //
  //b10 ~ normal(0, 1); //
  //b11 ~ normal(0, 1); //
  b12 ~ normal(0, 1); //
  //b13 ~ normal(0, 1); //
  //b14 ~ normal(0, 1); //
  b15 ~ normal(0, 1); //
  //b16 ~ normal(0, 1); //
  b17 ~ normal(0, 1); //
  b18 ~ normal(0, 1); //
  b19 ~ normal(0, 1); //
  //tau ~ gamma(0.01, 0.01) ; // prior for tau, BC***
  tau ~ gamma(0.01, 0.01); // prior for white noise process error, BC***
  phi ~ normal(0,0.5); // prior for autocorrelation coefficient, BC*** - changed to normal prior to see if it fixes divergent chains issue
  log_resid0 ~ normal(0, tau_red); // prior for initial residual for autocorrelation, BC***
  }

generated quantities {
  vector[N] pp_log_R;
  vector[N] pp_R;
  //vector[N] log_lik; 
  for (i in 1:N) {
    pp_log_R[i] = normal_rng(log_R_mean2[i], tau); // generate predicted log_recruits
    pp_R[i] = exp(pp_log_R[i]); // exponentiate predicted log_recruits to get untransformed predicted recruits
    //log_lik[i] = normal_lpdf(log_R[i] | log_R_mean2[i], tau); // generate log likelihoods - SOMEHOW THIS ISN'T WORKING 
    }
  }
