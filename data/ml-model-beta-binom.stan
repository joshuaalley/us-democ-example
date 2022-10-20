// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy
// binomial model- aggregate binary res at state-year level

// data
data {
  int<lower = 0> N; // number of obs = S with sum
  int<lower = 0> n_res[N]; // number of respondents in group
  int y[N]; // outcome: sum by group
  
  int<lower = 0> S; // number of countries
  int<lower = 1, upper = S> state[N]; // state indicator

  int<lower = 0> T; // number of year obs
  int<lower = 1, upper = T> year[N];
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual-level reg matrix
  int<lower = 1> J; // number of state-year variables
  matrix[N, J] Z; // state-year reg matrix; summed 
  int<lower = 1> L; // number of system-year variables
  matrix[T, L] G; // year/system reg matrix
  
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma; // dispersion of probabilities
  vector[N] theta; // non-centered implementation
  

  vector[S] alpha_state_std; // state intercepts- noncenter
  real<lower = 0> sigma_state; // state var. hyperparam
  real mu_state;
  

  vector[T] alpha_year_std; // state intercepts- noncenter
  real<lower = 0> sigma_year; // state var. hyperparam

  vector[I] beta; // individual coefs
  vector[J] gamma; // state-year coefs
  vector[L] lambda; // year coefs

}

transformed parameters {
   vector[T] mu_year; 
   vector[T] alpha_year; // year intercepts
   vector[S] alpha_state; // state intercepts 
   vector[N] mu;
   
   // regression model of mean year varying intercepts
   mu_year = G * lambda; // year/system level 

  // non-centered parameterization of state intercepts
  alpha_state = mu_state + sigma_state * alpha_state_std;

  // non-centered parameterization of year intercepts
  alpha_year = mu_year + sigma_year * alpha_year_std;
  
  // predict outcome
  mu = alpha +
      alpha_state[state] + alpha_year[year] +
       X * beta + Z * gamma;


}

// model 
model {
  
  // define priors
  alpha ~ std_normal(); // overall intercept
  sigma ~ normal(0, 1); // spread of success chances
  theta ~ normal(mu, sigma); 
  
  
  // state parameters
  // alpha_state ~ normal(mu_state, sigma_state); 
  sigma_state ~ normal(0, 1); // half-normal
  alpha_state_std ~ std_normal(); // state intercepts non-centered
  mu_state ~ normal(0, 1); 
  // year paramters
  // alpha_year ~ normal(mu_year, sigma_year); 
  alpha_year_std ~ std_normal(); // year intercepts non-centered
  sigma_year ~ normal(0, 1); // half-normal
  
  // regression coef priors 
  // robust priors 
  beta ~ student_t(5, 0, 1); 
  gamma ~ student_t(5, 0, 1); 
  lambda ~ student_t(5, 0, 1);
  
y ~ binomial_logit(n_res, theta); 

}

