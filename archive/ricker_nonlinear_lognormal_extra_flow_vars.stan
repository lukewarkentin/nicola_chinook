// see https://www.poissonconsulting.ca/analyses/2016/04/15/duncan-lardeau-juvenile-rainbow-abundance-16.html 
data {
  int N; // number rows
  vector[N] R; // recruits
  vector[N] S; // spawners
  vector[N] OSA; // ocean survival anomaly of hatchery fish
  vector[N] max_flow_fall; // maximum winter flow 
  vector[N] mean_flow_aug; //  mean august flow in rearing year
  vector[N] mean_jul_flow; // mean july flow in spawning year
  vector[N] mean_sep_oct_flow; // mean fall flow in spawning year
  vector[N] max_jan_feb_flow; // max flow in jan and feb in incubation winter
  }

parameters {
  real alpha; // alpha term
  real beta; // beta term
  real b1; // effect of ocean survival anomaly
  real b2; // effect of max winter flow
  real b3; // effect of mean aug flow
  real b4;// effect of july flow in spawning year
  real b5;// effect of fall flow in spawning year
  real b6; // effect of jan feb max flow in incubation year
  real sigma; // standard deviation
  }

transformed parameters {
  vector[N] mu; // predicted mean recruits - this needs to be zero-bounded; either log-normal or negative binomial
  mu = alpha * S .* exp(-beta * S + OSA * b1 + max_flow_fall * b2 + mean_flow_aug * b3 + mean_jul_flow * b4 + mean_sep_oct_flow * b5 + max_jan_feb_flow * b6); // model predicting log_r_per_s from alpha and beta and ocean survival anomaly, max winter flow, and drought index
  }

model {
  R ~ lognormal(log(mu), sigma); // likelihood function for linear model, used form from Poisson Consulting post
  alpha ~ normal(2, 2) T[0,]; // prior for alpha truncated at 0: maximum number of recruits per spawner at low stock size OR slope of line at origin
  beta ~ normal(0.002, 0.001); // prior for beta: rate of decrease - normal distribution had trouble getting to small value of beta
  b1 ~ normal(0, 1); // prior for slope of effect of ocean survival anomaly
  b2 ~ normal(0, 1); // prior for slope of effect of winter flooding
  b3 ~ normal(0, 1); // prior for slope of effect of drought index
  b4 ~ normal(0, 1); // prior for slope of effect of july flow
  b5 ~ normal(0, 1); // prior for slope of effect of fall spawning flow
  b6 ~ normal(0, 1); // prior for slope of effect of jan feb flow
  sigma ~ cauchy(0, 2) ; // prior for standard deviation - Poisson consulting used uniform(0,5) for prior
  }

generated quantities {
  vector[N] pp_R;
  vector[N] log_lik;
  for (i in 1:N) {
    pp_R[i] = lognormal_rng(log(mu[i]), sigma); // generate predicted recruits
    log_lik[i] = lognormal_lpdf(R[i] | log(mu[i]), sigma); // generate log likelihoods
    }
  }