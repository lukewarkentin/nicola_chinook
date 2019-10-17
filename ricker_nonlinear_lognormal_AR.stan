// see https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html 
data {
  int N; // number rows
  vector[N] R; // recruits
  vector[N] SW; // wild spawners
  vector[N] SH; // hatchery spawners
  vector[N] OSA; // ocean survival anomaly of hatchery fish
  vector[N] max_flow_fall; // maximum winter flow 
  vector[N] mean_flow_aug; //  mean august flow
  }

parameters {
  real alpha; // alpha term
  real betaW; // beta term for wild spawners
  real betaH; // beta term for hatchery spawners
  real b1; // effect of ocean survival anomaly
  real b2; // effect of max winter flow
  real b3; // effect of mean aug flow
  real tau; // white noise process error - see Atnarko code, by Brennan Connors (BC), BC***
  real phi; // autocorrelation coeffiecient, BC***
  real resid0; // starting residual for autocorrelation, BC***
  }

transformed parameters {
  vector[N] mu1; // predicted mean recruits - this needs to be zero-bounded; either log-normal or negative binomial
  vector[N] mu2; // predicted mean recruits with autocorrelation
  vector[N] resid; // residuals vector
  real tau_red; // tau * (1 - phi^2) 
  
  mu1 = alpha * (SW+SH) .* exp(-betaW * SW - betaH * SH + OSA * b1 + max_flow_fall * b2 + mean_flow_aug * b3); // model predicting recruits from alpha and beta and ocean survival anomaly, max winter flow, and drought index
  
  mu2[1] = mu1[1]; //+ phi * resid0; // autocorrelation time step 1, BC***
  for(i in 2:N) { // autocorrelation, all other time steps, BC***
    mu2[i] = mu1[i] + phi * resid[i-1] ; 
    } 

  resid = R - mu1; // residual for autocorrelation
  
  tau_red = tau * (1 - phi * phi); // standard deviation for prior for initial residual, BC***

  }

model {
  R ~ lognormal(log(mu2), tau); // likelihood function for linear model, used form from Poisson Consulting post
  alpha ~ normal(2, 2) T[0,]; // prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
  betaW ~ normal(0.002, 0.001); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  betaH ~ normal(0.002, 0.001); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  b1 ~ normal(0, 1); // prior for slope of effect of ocean survival anomaly
  b2 ~ normal(0, 1); // prior for slope of effect of winter flooding
  b3 ~ normal(0, 1); // prior for slope of effect of drought index
  tau ~ gamma(0.01, 0.01) ; // prior for tau, BC***
  phi ~ uniform(-0.99, 0.99); // prior for autocorrelation coefficient, BC***
  resid0 ~ normal(0, tau_red); // prior for initial residual for autocorrelation, BC***
  }

generated quantities {
  vector[N] pp_R;
  vector[N] log_lik;
  for (i in 1:N) {
    pp_R[i] = lognormal_rng(log(mu2[i]), tau); // generate predicted recruits
    log_lik[i] = lognormal_lpdf(R[i] | log(mu2[i]), tau); // generate log likelihoods
    }
  }
