// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy

// data
data {
  int<lower = 0> N; // number of individual obs
  vector[N] y; // outcome
  
  int<lower = 0> S; // number of state-year obs
  int<lower = 1, upper = S> state[N]; // state indicator
  
  int<lower = 0> T; // number of year obs
  int<lower = 1, upper = T> year[N];
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual  
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma; // outcome variance
//  real shape; // shape of skew normal
  
  vector[S] alpha_state_std; // state intercepts
  real<lower = 0> sigma_state; // state var. hyperparam 
  real mu_state; // state mean hyperparam 
  
  vector[T] alpha_year_std; // state intercepts
  real<lower = 0> sigma_year; // state var. hyperparam 
  real mu_year; // state mean hyperparam 
  
  vector[I] beta; // individual variable coefficients
}

transformed parameters {
  vector[T] alpha_year; 
  vector[S] alpha_state; 
 
  // non-centered parameterization of state intercepts 
  alpha_state = mu_state + sigma_state * alpha_state_std;
  
    // non-centered parameterization of year intercepts 
  alpha_year = mu_year + sigma_year * alpha_year_std;

}

// model 
model {
  // define priors
  alpha ~ std_normal(); 
  sigma ~ normal(0, 1); // half-normal
 // shape ~ normal(0, 2);
  
  alpha_state_std ~ std_normal(); // state intercepts
  mu_state ~ normal(0, 2); // prior state mean
  sigma_state ~ normal(0, 1); // half-normal
  
  alpha_year_std ~ std_normal(); // state intercepts
  mu_year ~ normal(0, 2); // prior state mean
  sigma_year ~ normal(0, 1); // half-normal
  
  beta ~ normal(0, 3);
  
  y ~ normal(alpha + alpha_state[state] + alpha_year[year] +
             X * beta, sigma);
             
}

