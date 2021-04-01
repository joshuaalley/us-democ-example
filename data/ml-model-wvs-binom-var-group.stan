// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy
// binomial model- aggregate binary res at state-year level
// varying slopes

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
  matrix[N, J] Z; // state-year reg matrix; 
  int<lower = 1> L; // number of system-year variables
  matrix[N, L] G; // year/system reg matrix
  int<lower = 1> R; // numer of state variables (regional dummies)
  matrix[S, R] H; // state-reg matrix
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> phi; // dispersion param
  
  vector[T] alpha_year_std; // state intercepts- noncenter
  real<lower = 0> sigma_year; // state var. hyperparam
  real mu_year; 

  vector[I] beta; // individual coefs
  vector[J] gamma; // state-year coefs
  matrix[L, R] eta; // state coefs
  //vector<lower=0,upper=pi()/2>[L] tau_unif;  // prior scale
  vector<lower = 0>[L] tau_lambda; // mean of lambda par in multivariate distribution 
  matrix[L, S] z_lambda; // for non-centered Cholesky factorization 
  cholesky_factor_corr[L] L_Omega_lambda; // for non-centered Cholesky factorization 

}

transformed parameters {
   vector[T] alpha_year; // year intercepts
   real<lower = 2> theta; // shape parameter
   // matrix[L, S] mu_lambda; // mean of year-level coefficients
   matrix[L, S] lambda; 
   //vector[S] alpha_state; // state intercepts
   
   // mean of lambda as a function of region 
   //mu_lambda = ; 
   
   // Cholesky factorization
   // varying slopes in year-level regression parameters lambda 
   // group-level vars H w/ parameters eta
   // state-specific intercepts included in G
   lambda = eta * H' + (diag_pre_multiply(tau_lambda, L_Omega_lambda) * z_lambda);
  
  // scales
  //tau_lambda = 2.5 * tan(tau_unif); // var slopes
  theta = phi + 2; // beta-binom

  // non-centered parameterization of year intercepts
  alpha_year = mu_year + sigma_year * alpha_year_std;
  

}

// model 
model {

  vector[N] year_s;
  vector[N] mu; 
  
  // define priors
  alpha ~ std_normal(); 
  
  // // state parameters
  // sigma_state ~ normal(0, 1); // half-normal
  // alpha_state_std ~ std_normal(); // state intercepts non-centered
  // year paramters
  alpha_year_std ~ std_normal(); // year intercepts non-centered
  sigma_year ~ normal(0, 1); // half-normal
  
  // regression coef priors 
  // robust priors 
  beta ~ student_t(5, 0, 1); 
  gamma ~ student_t(5, 0, 1); 
  to_vector(z_lambda) ~ normal(0, .25);
  L_Omega_lambda ~ lkj_corr_cholesky(2);
  tau_lambda ~ normal(0, .25); 
  to_vector(eta) ~ normal(0, 1);
 
  // year shocks and probability of success
  for(i in 1:N){
  year_s[i] = G[i, ] * lambda[, state[i]];
  mu[i] = inv_logit(alpha +
       alpha_year[year[i]] +
       X[i] * beta + Z[i] * gamma + year_s[i]);
  }

       
y ~ beta_binomial(n_res, 
         mu * theta, // alpha in beta_binom
         (1-mu) * theta); // beta in beta_binom

}

