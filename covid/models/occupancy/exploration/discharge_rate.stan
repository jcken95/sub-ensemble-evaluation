// TODO
// make functions
// change back to beta regression
// work out prior
// give independence for each region
// make random effect foe each region

data {
  int < lower = 1 > N; // Sample size
  int < lower = 1 > T; // Max number of time steps
  int < lower = 1 > R; // Max number of region
  int < lower = 1 > J; // Max Length of Stay lookback
  array[N] int<lower=1, upper=T> day;
  array[N] int<lower=1, upper=R> region;
  vector[N] y; // occupancy rate
  vector[N] x; // admissions rate
}

transformed data {
  array[R, T] real occupancy_regional = rep_array(0, R, T);
  array[R, T] real admissions_regional = rep_array(0, R, T);

  // convert to 2D array
  for (n in 1:N) {
    occupancy_regional[region[n], day[n]] += y[n];
    admissions_regional[region[n], day[n]] += x[n];
  }
}


parameters {
  //real intercept; // including this as I think there's some "eternal" beds being occupied
  // we want to parameterise a lognormal_cdf to be our discharge probability over j days stayed
  real<lower=0> lognormal_mu;
  real<lower=0> lognormal_sigma;
  real<lower=0> beta; // beta distribution shape
}


model {

  vector[J] h; // recent history of admissions for a given time
  vector[J] f; // recent history minus discharge
  real mu; //
  real alpha; // beta shape



  lognormal_mu ~ cauchy(1,2); // prior on discharge distribution
  lognormal_sigma ~ cauchy(1,2); // prior on discharge distribution
  //beta ~ normal(0, 100);

  // iterate over the regions
  for (r in 1:R) {
  // iterate over the time series, excluding first lookback length
  for (i in 1+J:T) {

    // iterate over lookback period for a given time point
    for (j in 1:J) {

      // calculate how many patients at this lookback
      h[j] = admissions_regional[r, i-j+1];

      // estimate how many patients remain after removing discharged patients
      f[j] = h[j] - h[j] * lognormal_cdf(j-1 | lognormal_mu, lognormal_sigma);

    }

    mu = sum(f);

    // we need alpha not mu for the beta error
    alpha = - (beta * mu) / (mu - 1);

    // fit on the sum of remaining patients
    //occupancy_regional[r, i] ~ beta(alpha, beta);
    occupancy_regional[r, i] ~ normal(mu, beta);

  }

  }
}

generated quantities {

  array[R, T] real y_hat = rep_array(0, R, T);
  vector[J] h_hat; // recent history of admissions for a given time
  vector[J] f_hat; // recent history minus discharge
  real mu; //
  real alpha; // beta shape

  for (r in 1:R) {



  for (i in 1+J:T) {

    for (j in 1:J) {

      {

      h_hat[j] = admissions_regional[r, i-j+1];
      f_hat[j] = h_hat[j] - h_hat[j] * lognormal_cdf(j-1 | lognormal_mu, lognormal_sigma);

    }

    }

    mu = sum(f_hat);

    alpha = -(beta * mu) / (mu - 1);

    // fit on the sum of remaining patients
    // mean = alpha / beta, therefore we can get alpha from sum(f)
    //y_hat[r, i] = beta_rng(alpha, beta);
    y_hat[r, i] = normal_rng(mu, beta);

  }

  // why isn't this working??
  for (i in 1:J) {
    y_hat[r, i] = 0;

  }

  }

}
