// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy
// binomial model- aggregate binary res at state-year level
// varying individual slopes by state
// no state-intercepts- perfect collinearity

// data
data {
  int<lower = 0> N; // number of obs = S with sum
  int<lower = 0> n_res[N]; // number of respondents in group
  int y[N]; // outcome: sum by group

  int<lower = 0> T; // number of year obs
  int<lower = 1, upper = T> year[N];
  
  int<lower = 0> S; // number of countries
  int<lower = 1, upper = S> state[N]; // state indicator
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual-level reg matrix
  int<lower = 1> J; // number of state-year variables
  matrix[N, J] Z; // state-year reg matrix; 
  int<lower = 1> L; // number of system-year variables
  matrix[T, L] G; // year/system reg matrix
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma;
  vector[N] theta; // odds of success 
 
  vector[T] alpha_year_std; // state intercepts- noncenter
  real<lower = 0> sigma_year; // state var. hyperparam

  vector[J] gamma; // state-year coefs
  vector[L] lambda; // year-level coefficients
  matrix[I, S] mu_beta; // mean of indiv-level coefficients
  vector<lower = 0>[I] tau_beta; // scale of lambda par in multivariate distribution 
  matrix[I, S] z_beta; // for non-centered Cholesky factorization 
  cholesky_factor_corr[I] L_Omega_beta; // for non-centered Cholesky factorization 

}

transformed parameters {
   vector[T] alpha_year; // year intercepts
   matrix[I, S] beta;
   vector[N] mu;   
   vector[T] mu_year; 
   
   
   // regression model of mean year varying intercepts
   mu_year = G * lambda; // year/system level 

  // non-centered parameterization of year intercepts
  alpha_year = mu_year + sigma_year * alpha_year_std;
   
   // Cholesky factorization
   // varying slopes in year-level regression parameters lambda 
   // state-specific intercepts included in G
   beta = mu_beta + (diag_pre_multiply(tau_beta, L_Omega_beta) * z_beta);
  
  
   
  // loop over observations
  for(i in 1:N){
  mu[i] = alpha +
       alpha_year[year[i]] +  
       X[i, ] * beta[, state[i]] +
       Z[i, ] * gamma;
  }
  

}

// model 
model {
  
  // define priors
  // phi ~ exponential(1);
  alpha ~ std_normal(); // overall intercept
  sigma ~ normal(0, .5); // spread of success chances
  theta ~ normal(mu, sigma); 
  
  // year parameters
  alpha_year_std ~ std_normal(); // year intercepts non-centered
  sigma_year ~ normal(0, 1); // half-normal
  mu_year ~ normal(0, 1);
  
  // regression coef priors 
  // robust priors 
  lambda ~ student_t(5, 0, 1); 
  gamma ~ student_t(5, 0, 1); 
  to_vector(z_beta) ~ normal(0, 1);
  L_Omega_beta ~ lkj_corr_cholesky(2);
  tau_beta ~ normal(0, 1); 
  to_vector(mu_beta) ~ normal(0, 1);

       
y ~ binomial_logit(n_res, theta); 

}


