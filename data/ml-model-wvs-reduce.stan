// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy


// functions {
//   real partial_sum(real[] y_slice,
//                    int start, int end,
//                    matrix X, vector beta,
//                    int[] state,
//                    vector alpha_state,
//                    int[] year,
//                    vector alpha_year,
//                    real alpha,
//                    real omega,
//                    real sigma) {
//     return skew_normal_lpdf(y_slice | alpha +
//       alpha_state[state[start:end]] + alpha_year[year[start:end]] +
//       X[start:end, ] * beta, omega, sigma);
//   }
// }

// data
data {
  int<lower = 0> N; // number of individual obs
  int y[N]; // outcome
  int obs_index[N]; // index observation
  
  int<lower = 0> S; // number of state-year obs
  int<lower = 1, upper = S> state[N]; // state indicator

  int<lower = 0> T; // number of year obs
  int<lower = 1, upper = T> year[N];
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual-level reg matrix
  int<lower = 0> n_uX; // unique indiv obs 
  matrix[n_uX, I] uX; // unique matrix entries
  
  int<lower = 1> J; // number of state-year groups
  matrix[S, J] Z; // state-year reg matrix
  int<lower = 1> L; // number of system-year groups
  matrix[T, L] G; // year/system reg matrix
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> omega; // scale
  real sigma; // shape param
 
vector[S] alpha_state_std; // state intercepts- noncenter
real<lower = 0> sigma_state; // state var. hyperparam

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
   vector[S] mu_state;
   
   // regression models of state and year varying intercepts
   mu_year = G * lambda; // year/system level 
   mu_state = Z * gamma; // state-year level

  // non-centered parameterization of state intercepts
  alpha_state = mu_state + sigma_state * alpha_state_std;

  // non-centered parameterization of year intercepts
  alpha_year = mu_year + sigma_year * alpha_year_std;


}

// model 
model {
  // define grain size (automatic selection)
  // int grainsize = 1; 
   vector[n_uX] x_beta;
  
  // define priors
  alpha ~ std_normal(); 
  
  // state parameters
  sigma_state ~ normal(0, 1); // half-normal
  alpha_state_std ~ std_normal(); // state intercepts non-centered
  // year paramters
  alpha_year_std ~ std_normal(); // year intercepts non-centered
  sigma_year ~ normal(0, 1); // half-normal
  
  // regression coef priors 
  // robust priors for indiv params and 
  //weak info normal for state-year level
  beta ~ normal(0, 2); 
  gamma ~ normal(0, 2); 
  lambda ~ normal(0, 2);
  
  // reduced redundant computation of design matrix X
	x_beta = uX * beta;
  // need a way to expand this to length N 
  // and tie indexes to it
  // match uX to X and mark each similar one?
  
  y ~ skew_normal(y, alpha +
      alpha_state[state] + alpha_year[year] +
      x_beta[obs_index], omega, sigma)
  
//  // split outcome w/ reduce sum
// target += reduce_sum(partial_sum,
//                       y,
//                       grainsize,
//                       X, beta,
//                       state, alpha_state,
//                       year, alpha_year,
//                       alpha,
//                       omega,
//                       gamma);
}

