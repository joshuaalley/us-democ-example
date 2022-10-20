// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy
// binomial model- aggregate binary res at state-year level

// data
data {
  int<lower = 0> N; // number of obs = S with sum
  int<lower = 0> n_res[N]; // number of respondents in group
  int y[N]; // outcome: sum by group
  
  int<lower = 0> S; // number of state-year obs
  int<lower = 1, upper = S> state[N]; // state indicator

  int<lower = 0> T; // number of year obs
  int<lower = 1, upper = T> year[N];
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual-level reg matrix
  int<lower = 1> J; // number of state-year groups
  matrix[S, J] Z; // state-year reg matrix
  int<lower = 1> L; // number of system-year groups
  matrix[T, L] G; // year/system reg matrix
}

// parameters.
parameters {
  real alpha; // overall intercept
//  real<lower = 0> sigma; // outcome variance
  
 
  vector[S] alpha_state_std; // state intercepts- noncenter
  real<lower = 0> sigma_state; // state var. hyperparam
  real mu_state;
  
  vector[T] alpha_year_std; // state intercepts- noncenter
  real<lower = 0> sigma_year; // state var. hyperparam

  vector[I] beta; // individual coefs
  vector[J] gamma; // state-year coefs
  vector[L] lambda; // alliance-year coefs

}

transformed parameters {
   vector[T] mu_year; 
   vector[T] alpha_year; // year intercepts
   vector[S] alpha_state; // state intercepts

   
   // regression models of mean year varying intercepts
   mu_year = G * lambda; // year/system level 

  // non-centered parameterization of state intercepts
  alpha_state = mu_state + sigma_state * alpha_state_std;

  // non-centered parameterization of year intercepts
  alpha_year = mu_year + sigma_year * alpha_year_std;
  

}

// model 
model {
  // define grain size (automatic selection)
  int grainsize = 1; 
  
  // define priors
  alpha ~ std_normal(); 
  
  // state parameters
  sigma_state ~ normal(0, 1); // half-normal
  alpha_state_std ~ std_normal(); // state intercepts non-centered
  // year paramters
  alpha_year_std ~ std_normal(); // year intercepts non-centered
  sigma_year ~ normal(0, 1); // half-normal
  
  // regression coef priors 
  // robust priors 
  beta ~ student_t(5, 0, 1); 
  gamma ~ student_t(5, 0, 1); 
  lambda ~ student_t(5, 0, 1);

  y ~ binomial_logit(n_res, alpha +
      alpha_state[state] + alpha_year[year] +
       X * beta + Z * gamma);

}

