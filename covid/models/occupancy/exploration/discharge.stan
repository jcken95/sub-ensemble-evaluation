// TODO
// make functions

data {
  int < lower = 1 > N; // Sample size
  int < lower = 1 > J; // Max Length of Stay lookback
  int < lower = 1 > T;
  vector[N] y; // occupancy
  vector[N] x; // admissions
}

parameters {
  //real intercept; // including this as I think there's some "eternal" beds being occupied
  // we want to parameterise a lognormal_cdf to be our discharge probability over j days stayed
  real<lower=0> lognormal_mu;
  real<lower=0> lognormal_sigma;
  real<lower=0> beta; // shape of gamma error distribution
}

model {

  vector[J] h; // recent history of admissions for a given time
  vector[J] f; // recent history minus discharge


  lognormal_mu ~ cauchy(1,2); // prior on discharge distribution
  lognormal_sigma ~ cauchy(1,2); // prior on discharge distribution
  beta ~ normal(0, 1); // prior on error shape parameter


  // iterate over the time series, excluding first lookback length
  for (i in 1+J:T) {

    // iterate over lookback period for a given time point
    for (j in 1:J) {

      // calculate how many patients at this lookback
      h[j] = x[i-j+1];

      // estimate how many patients remain after removing discharged patients
      f[j] = h[j] - h[j]*lognormal_cdf(j-1 | lognormal_mu, lognormal_sigma);

    }

    // fit on the sum of remaining patients
    // mean = alpha / beta, therefore we can get alpha from sum(f)*
    y[i] ~ gamma(sum(f)*beta, beta);

  }
}

generated quantities {

  vector[N] y_hat; // predicted output
  vector[J] h_hat; // recent history of admissions for a given time
  vector[J] f_hat; // recent history minus discharge

  for (i in 1:J) {
    y_hat[i] = y[i];

  }

  for (i in 1+J:T) {

    for (j in 1:J) {

      {
      h_hat[j] = x[i-j+1];
      f_hat[j] = h_hat[j] - h_hat[j]*lognormal_cdf(j-1 | lognormal_mu, lognormal_sigma);
    }

    }

    y_hat[i] = gamma_rng(sum(f_hat)*beta, beta);


  }

}
