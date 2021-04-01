# Joshua Alley and John Owen
# Analysis of relationship between US success
# and public support for democracy abroad


# data at three levels
# imputed.data.wvs: individual level- 391754 respondents
# imputed.state.yr; state-year level- 269 state-year obs
# us.data.final: year/system level- 29 years

# potential redundant calculation gains
nrow(imputed.data.wvs[[1]][, 5:13])
nrow(distinct(imputed.data.wvs[[1]][, 5:13]))

# create data list
stan.data <- list(
   N = nrow(imputed.data.wvs[[1]]),
   y = imputed.data.wvs[[1]]$high.democ,
   # state-year indicators
   state = imputed.data.wvs[[1]]$cntry.yr.id,
   S = length(unique(imputed.data.wvs[[1]]$cntry.yr.id)),
   # year/system indicators
   year = imputed.data.wvs[[1]]$year.id,
   T = length(unique(imputed.data.wvs[[1]]$year.id)),
   # individual level variables
   I = ncol(imputed.data.wvs[[1]][, 5:13]),
   X = imputed.data.wvs[[1]][, 5:13],
   uX = distinct(imputed.data.wvs[[1]][, 5:13]),
   n_uX = nrow(uX),
   # state level variables
   J = ncol(imputed.state.yr.final[[1]]),
   Z = imputed.state.yr.final[[1]],
   # year/US/system level vars
   L = ncol(us.data.final),
   G = us.data.final
)


# compile cmdstan model
stan.model.cmd <- cmdstan_model("data/ml-model-wvs-reduce.stan",
                                cpp_options = list(stan_threads = TRUE))

# run model- check code
stan.model.cmd$print()
stan.model.cmd$exe_file()

# quick check with variational approx: fits but does not converge
vb.wvs <- stan.model.cmd$variational(data = stan.data, seed = 123, output_samples = 1000)
vb.wvs$cmdstan_summary()
rm(vb.wvs) # remove 

# fit
system.time(
fit.wvs <- stan.model.cmd$sample(
             data = stan.data,
             seed = 12,
             iter_warmup = 1000,
             iter_sampling = 1000,
             chains = 4,
             parallel_chains = 4,
             threads_per_chain = 2,
             refresh = 200,
             max_treedepth = 20,
             adapt_delta = .9
   )
)
# save outside of workspace
fit.wvs$save_object(file = "data/fit-wvs.RDS")

# diagnose
fit.wvs$cmdstan_diagnose()
fit.wvs$cmdstan_summary()

diagnostics_df <- as_draws_df(fit.wvs$sampler_diagnostics())
print(diagnostics_df)

# autocorrelation
mcmc_acf(fit.wvs$draws("alpha"))

mcmc_pairs(fit.wvs$draws(),
           pars = c("alpha", "sigma_state", 
                    "sigma_year"),
           off_diag_args = list(size = 0.75))

# summarize parameters
fit.wvs$summary("alpha")
mcmc_hist(fit.wvs$draws("alpha"))

# individual-level regressors
fit.wvs$summary("beta")
mcmc_hist(fit.wvs$draws("beta"))

# state-level parameters
fit.wvs$summary("gamma")
mcmc_intervals(fit.wvs$draws("gamma"))
# state-year intercepts
fit.wvs$summary(c("sigma_state", "alpha_state"))
mcmc_intervals(fit.wvs$draws("alpha_state"))

# individual-level parameters
fit.wvs$summary("lambda")
mcmc_intervals(fit.wvs$draws("lambda"))
# year intercepts
fit.wvs$summary(c("sigma_year", "alpha_year"))
mcmc_intervals(fit.wvs$draws("alpha_year"))



