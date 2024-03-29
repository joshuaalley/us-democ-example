// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy
// binomial model- aggregate binary res at state-year level
// varying slopes by groups of states
// no state-intercepts- perfect collinearity

// data
data {
  int<lower = 0> N; // number of obs = S with sum
  int<lower = 0> n_res[N]; // number of respondents in group
  int y[N]; // outcome: sum by group

  int<lower = 0> T; // number of year obs
  int<lower = 1, upper = T> year[N];
  
  int<lower = 0> R; // number of regions
  int<lower = 1, upper = R> group[N]; 
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual-level reg matrix
  int<lower = 1> J; // number of state-year variables
  matrix[N, J] Z; // state-year reg matrix; 
  int<lower = 1> L; // number of system-year variables
  matrix[N, L] G; // year/system reg matrix
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> sigma;
  vector[N] theta; // odds of success 
 
  vector[T] alpha_year_std; // state intercepts- noncenter
  real<lower = 0> sigma_year; // state var. hyperparam
  real mu_year; 

  vector[I] beta; // individual coefs
  vector[J] gamma; // state-year coefs
  matrix[L, R] mu_lambda; // mean of year-level coefficients
  vector<lower = 0>[L] tau_lambda; // scale of lambda par in multivariate distribution 
  matrix[L, R] z_lambda; // for non-centered Cholesky factorization 
  cholesky_factor_corr[L] L_Omega_lambda; // for non-centered Cholesky factorization 

}

transformed parameters {
   vector[T] alpha_year; // year intercepts
   matrix[L, R] lambda;
   vector[N] year_r;
   vector[N] mu; 
   
   // Cholesky factorization
   // varying slopes in year-level regression parameters lambda 
   // state-specific intercepts included in G
   lambda = mu_lambda + (diag_pre_multiply(tau_lambda, L_Omega_lambda) * z_lambda);
   
  // non-centered parameterization of year intercepts
  alpha_year = mu_year + sigma_year * alpha_year_std;
  
  
    // year shocks and probability of success
  for(i in 1:N){
  year_r[i] = G[i, ] * lambda[, group[i]];
  }
  
  mu = alpha +
       alpha_year[year] + // alpha_state[state] + 
       X * beta + Z * gamma + year_r;
  

}

// model 
model {
  
  // define priors
  // phi ~ exponential(1);
  alpha ~ std_normal(); // overall intercept
  sigma ~ normal(0, 1); // spread of success chances
  theta ~ normal(mu, sigma); 
  
  // year parameters
  alpha_year_std ~ std_normal(); // year intercepts non-centered
  sigma_year ~ normal(0, 1); // half-normal
  mu_year ~ normal(0, 1);
  
  // state parameters
  //alpha_state_std ~ std_normal(); // year intercepts non-centered
  //sigma_state ~ normal(0, 1); // half-normal
  
  // regression coef priors 
  // robust priors 
  beta ~ student_t(5, 0, 1); 
  gamma ~ student_t(5, 0, 1); 
  to_vector(z_lambda) ~ normal(0, 1);
  L_Omega_lambda ~ lkj_corr_cholesky(2);
  tau_lambda ~ normal(0, 1); 
  to_vector(mu_lambda) ~ normal(0, 1);

       
y ~ binomial_logit(n_res, theta); 

}


