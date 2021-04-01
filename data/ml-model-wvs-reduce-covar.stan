// Joshua Alley and John Owen
// Multilevel model of global attitudes towards democracy
// Places state and year-level vars in individual parameter vector


functions {
  real partial_sum(real[] y_slice,
                   int start, int end,
                   matrix X, vector beta,
                   int[] state,
                   vector alpha_state,
                   //int[] year,
                   //vector alpha_year,
                   real alpha,
                   real omega,
                   real gamma) {
    return skew_normal_lpdf(y_slice | alpha +
      alpha_state[state[start:end]] + //alpha_year[year[start:end]] +
      X[start:end, ] * beta, omega, gamma);
  }
}

// data
data {
  int<lower = 0> N; // number of individual obs
  real y[N]; // outcome
  
  int<lower = 0> S; // number of state-year obs
  int<lower = 1, upper = S> state[N]; // state indicator

  //int<lower = 0> T; // number of year obs
  //int<lower = 1, upper = T> year[N];
  
  int<lower = 1> I; // number of individual variables
  matrix[N, I] X; // individual-level reg matrix
}

// parameters.
parameters {
  real alpha; // overall intercept
  real<lower = 0> omega; // scale
  real gamma; // shape param
//  real<lower = 0> sigma; // outcome variance
 
vector[S] alpha_state_std; // state intercepts- noncenter
real<lower = 0> sigma_state; // state var. hyperparam
real mu_state; // mean hyperparam 

// vector[T] alpha_year_std; // state intercepts- noncenter
// real<lower = 0> sigma_year; // state var. hyperparam
// real mu_year; // mean hyperparam

vector[I] beta; // regression coefs

}

transformed parameters {
   //vector[T] alpha_year; // year intercepts
   vector[S] alpha_state; // state intercepts

  // non-centered parameterization of state intercepts
  alpha_state = mu_state + sigma_state * alpha_state_std;

  // non-centered parameterization of year intercepts
  //alpha_year = mu_year + sigma_year * alpha_year_std;


}

// model 
model {
  // define grain size (automatic selection)
  int grainsize = 1; 
  
  // define priors
  alpha ~ std_normal(); 
  omega ~ normal(0, 1); // half-normal
  gamma ~ normal(0, 2); // shape param
  
  // state parameters
  sigma_state ~ normal(0, 1); // half-normal
  alpha_state_std ~ std_normal(); // state intercepts non-centered
  mu_state ~ normal(0, .5); // mean hyperparam
  // year paramters
  //alpha_year_std ~ std_normal(); // year intercepts non-centered
  //sigma_year ~ normal(0, 1); // half-normal
  //mu_year ~ normal(0, .5); // mean hyperparam
  
  // regression coef priors 
  // robust priors 
  beta ~ normal(0, 2); 

  
 // split outcome w/ reduce sum
target += reduce_sum(partial_sum,
                      y,
                      grainsize,
                      X, beta,
                      state, alpha_state,
                      //year, alpha_year,
                      alpha,
                      omega,
                      gamma);
}
