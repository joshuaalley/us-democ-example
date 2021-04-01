# Joshua Alley and John Owen
# compare binomial model fits with and without year-level variables


# will compare fit on the first imputed dataset

### Model with year-level variables
# set up stan data 
# create data list
stan.data.binom <- list(
  N = nrow(imputed.wvs.sum[[1]]),
  n_res = imputed.wvs.sum[[1]]$n.res,
  y = imputed.wvs.sum[[1]]$high.democ.sum,
  # state-year indicators
  state = imputed.wvs.sum[[1]]$cntry.id,
  S = length(unique(imputed.wvs.sum[[1]]$cntry.id)),
  # year/system indicators
  year = imputed.wvs.sum[[1]]$year.id,
  T = length(unique(imputed.wvs.sum[[1]]$year.id)),
  # individual level variables
  I = ncol(imputed.wvs.sum[[1]][, 3:10]),
  X = imputed.wvs.sum[[1]][, 3:10],
  # state level variables
  J = ncol(imputed.state.yr.final[[1]][, 3:11]),
  Z = imputed.state.yr.final[[1]][, 3:11],
  # year/US/system level vars
  L = ncol(us.data.final),
  G = us.data.final
)


# stan model fit
system.time(
  fit.wvs.bin <- stan.model.cmd.bin$sample(
    data = stan.data.binom,
    seed = 12,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4,
    refresh = 200,
    max_treedepth = 20,
    adapt_delta = .95
  )
)

# diagnose
fit.wvs.bin$cmdstan_diagnose()
summary(as_draws_df(fit.wvs.bin$sampler_diagnostics()))
# create Rstan object for use with LOO package
# rstan.year <- rstan::read_stan_csv(fit.wvs.bin$output_files())



### data for calculating substantive effect of Trump presidency 
### simulate impact of increasing treaty depth 
year.data.lsim <- numeric(ncol(us.data.final))
names(year.data.lsim) <- c(colnames(us.data.final))

# Set values of variables for simulation 
year.data.lsim["constant"] <- 1 
year.data.lsim["growth_WDI_PW"] <- median(us.data.final$growth_WDI_PW)
year.data.lsim["v2x_libdem_VDEM"] <- median(us.data.final$v2x_libdem_VDEM)
year.data.lsim["fariss_hr"] <- median(us.data.final$fariss_hr)
year.data.lsim["perc.protest"] <- median(us.data.final$perc.protest)
year.data.lsim["gini_disp"] <- median(us.data.final$gini_disp)
year.data.lsim["rep_pres"] <- 0
year.data.lsim["trump"] <- 0
year.data.lsim["post_cold_war"] <- 1
year.data.lsim["chinese_growth"] <- median(us.data.final$chinese_growth)
year.data.lsim["war_outcome"] <- 0

# vector with high depth
year.data.hsim <- year.data.lsim
year.data.hsim["trump"] <- 1 # key IV: 3rd quartile

# state-level variables
# median of all: use first imputed dataset
state.vars.sum <- as.numeric(apply(imputed.state.yr.final[[1]][, 2:10], 2, median))
# median for individual variables 
indiv.vars.sum <- as.numeric(apply(imputed.wvs.sum[[1]][, 3:10], 2, median))

# multiply 
# Simulate the effect 
year.lsim <- as.matrix((select(draws.binom, starts_with("lambda")))) %*% 
  year.data.lsim
hist(year.lsim)
quantile(year.lsim, c(0.05, .95))
year.hsim <- as.matrix((select(draws.binom, starts_with("lambda")))) %*% 
  year.data.hsim
hist(year.hsim)
quantile(year.hsim, c(0.05, .95))

# Look at difference
year.diff <- year.hsim - year.lsim
mean(year.diff < 0) # 94% negative


# feed the rest into the likelihood
# trump dummy = 0 
pred.lsim <- draws.binom$alpha +
  year.lsim +
  (as.matrix(select(draws.binom, starts_with("gamma")))) %*% 
  state.vars.sum +
  (as.matrix(select(draws.binom, starts_with("beta")))) %*% 
  indiv.vars.sum 
summary(pred.lsim)
pred.lsim <- exp(pred.lsim) / (1 + exp(pred.lsim))
summary(pred.lsim)

# trump dummy = 1
pred.hsim <- draws.binom$alpha +
  year.hsim +
  (as.matrix(select(draws.binom, starts_with("gamma")))) %*% 
  state.vars.sum +
  (as.matrix(select(draws.binom, starts_with("beta")))) %*% 
  indiv.vars.sum 
summary(pred.hsim)
pred.hsim <- exp(pred.lsim) / (1 + exp(pred.hsim))
summary(pred.hsim)
# difference
pred.diff <- pred.lsim - pred.hsim
hist(pred.diff)
summary(pred.diff)
mean(pred.diff < 0) # 94% negative
quantile(pred.diff, c(0.05, .95))



### Model without year-level variables
# new data
stan.data.binom.noy <- list(
  N = nrow(imputed.wvs.sum[[1]]),
  n_res = imputed.wvs.sum[[1]]$n.res,
  y = imputed.wvs.sum[[1]]$high.democ.sum,
  # state-year indicators
  state = imputed.wvs.sum[[1]]$cntry.id,
  S = length(unique(imputed.wvs.sum[[1]]$cntry.id)),
  # year/system indicators
  year = imputed.wvs.sum[[1]]$year.id,
  T = length(unique(imputed.wvs.sum[[1]]$year.id)),
  # individual level variables
  I = ncol(imputed.wvs.sum[[1]][, 3:10]),
  X = imputed.wvs.sum[[1]][, 3:10],
  # state level variables
  J = ncol(imputed.state.yr.final[[1]][, 3:11]),
  Z = imputed.state.yr.final[[1]][, 3:11]
)

# compile model code
stan.model.bin.noy <- cmdstan_model("data/ml-model-wvs-binom-noy.stan")

# run model- check code
stan.model.bin.noy$print()
stan.model.bin.noy$exe_file()

# stan model fit
system.time(
  fit.wvs.bin.noy <- stan.model.bin.noy$sample(
    data = stan.data.binom.noy,
    seed = 12,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4,
    refresh = 200,
    max_treedepth = 20,
    adapt_delta = .95
  )
)


### compare with loo 
# with year-level variables
loo.y <- loo(fit.wvs.bin$draws("log_lik"))
print(loo.y)
plot(loo.y)
pareto.k.values  <- pareto_k_influence_values(loo.y)
plot(stan.data.binom$y / stan.data.binom$n_res, pareto.k.values)
# moment matches (need compiled rstan model)
loo.y <- loo_moment_match(fit.wvs.bin,
                          post_draws = fit.wvs.bin$draws(),
                          loo = loo.y)
plot(loo.y)
# without year-level variables
loo.ny <- loo(fit.wvs.bin.noy$draws("log_lik"), cores = 2,)
print(loo.ny)

# WAIC
waic.y <- waic(fit.wvs.bin$draws("log_lik"))
print(waic.y)
waic.ny <- waic(fit.wvs.bin.noy$draws("log_lik"))
print(waic.ny)
print(loo_compare(waic.y, waic.ny), digits = 3)


### Varying slopes model
# set up data: single for a test run
# create data list
stan.data.binom.var <- list(
  N = nrow(imputed.wvs.sum[[1]]),
  n_res = imputed.wvs.sum[[1]]$n.res,
  y = imputed.wvs.sum[[1]]$high.democ.sum,
  # state-year indicators
  # state = imputed.wvs.sum[[1]]$cntry.id,
  # S = length(unique(imputed.wvs.sum[[1]]$cntry.id)),
  # year/system indicators
  year = imputed.wvs.sum[[1]]$year.id,
  T = length(unique(imputed.wvs.sum[[1]]$year.id)),
  # individual level variables
  I = ncol(imputed.wvs.sum[[1]][, 3:10]),
  X = imputed.wvs.sum[[1]][, 3:10],
  # state level variables
  J = ncol(imputed.state.yr.final[[1]][, 3:11]),
  Z = imputed.state.yr.final[[1]][, 3:11],
  # year/US/system level vars
  L = ncol(us.data.single.long),
  G = us.data.single.long,
  # regional indicators
  R = length(unique(imputed.wvs.sum[[1]]$region)),
  region = imputed.wvs.sum[[1]]$region
)

# sampling fit
system.time(
  fit.wvs.bin.var <- stan.model.bin.var$sample(
    data = stan.data.binom.var,
    seed = 12,
    iter_warmup = 1000,
    iter_sampling = 1000,
    chains = 4,
    parallel_chains = 4,
    refresh = 200,
    max_treedepth = 20,
    adapt_delta = .95
  )
)
mcmc_intervals(fit.wvs.bin.var$draws("lambda"))

# compile model code: var slops w/ generated quantities block
stan.model.bin.noy <- cmdstan_model("data/ml-model-wvs-binom-var-gq.stan")


