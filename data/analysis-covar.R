# Joshua Alley and John Owen
# Analysis of relationship between US success
# and public support for democracy abroad

# all covariates on one level- continuous outcome

### take the state and system-year variables and places them on a single level

# create dataset at one level
us.data.single <- filter(us.data.five,
                         year %in% unique(state.year.data$year)) %>%
  mutate(
    # add presidential partisanship
    rep_pres = ifelse((year >= 1981 & year <= 1992) | # Reagan/Bush
                        (year >= 2001 & year <= 2008),  # HW Bush
                      1, 0),
    # and cold war
    post_cold_war = ifelse(year >= 1991, 1, 0),
    constant = 1
  ) %>% # add protest data
  left_join(gdelt.protests) %>% # select key
  select(year, growth_WDI_PW, perc.protest, v2x_libdem_VDEM,
         fariss_hr, gini_disp, 
         Clinton, W.Bush, Obama, Trump,
         chinese_growth, war_outcome) %>%
  rename(us_hr = fariss_hr,
         us_growth = growth_WDI_PW,
         us_ineq = gini_disp,
         us_vdem = v2x_libdem_VDEM)
glimpse(us.data.single)

# lag fariss HR: average of last two observed years
# us.data.single$us_hr[
#   is.na(us.data.single$us_hr)] <- (0.28316380 + 0.29497110) / 2

# rescale by 2sd 
us.data.single[, 2:12] <-  apply(us.data.single[, 2:12], 2, 
                                function(x) arm::rescale(x, binary.inputs = "0/1"))

# imputed state data rs
imputed.state.yr.rs <- vector(mode = "list", length = 20)
for(i in 1:length(imputed.state.yr)){
  # rescale by 2sd
  imputed.state.yr.rs[[i]] <- as.data.frame(apply(imputed.state.yr[[i]][, 4:12],
                                                  2, function(x) arm::rescale(x, binary.inputs = "0/1")))
  imputed.state.yr.rs[[i]] <- cbind.data.frame(select(imputed.state.yr[[i]],
                                                      ccode, year),
                                               imputed.state.yr.rs[[i]])
}  
glimpse(imputed.state.yr.rs[[1]])

# list of data frames
data.single <- vector(mode = "list", length = length(imputed.data.wvs))
for(i in 1:length(data.single)){
  data.single[[i]] <- left_join(imputed.data.wvs[[i]], imputed.state.yr.rs[[i]])
  data.single[[i]] <- left_join(data.single[[i]], us.data.single) %>%
    mutate(
      region = ifelse(ccode < 200, 1, # Americas 
                      ifelse(ccode %in% 200:400, 2, # Europe
                             ifelse(ccode > 400 & ccode < 600, 3, # subs Africa
                                    ifelse(ccode >= 600 & ccode < 700, 4, # MENA
                                           ifelse(ccode > 700, 5, 0))))) # Asia
    )
}

# take 5 at random
data.single <- sample(data.single, size = 5, replace = FALSE)

# create data list
stan.data.single <- list(
  N = nrow(data.single[[1]]),
  y = data.single[[1]]$agg.democ,
  # state-year indicators
  state = data.single[[1]]$cntry.id,
  S = length(unique(data.single[[1]]$cntry.id)),
  # year/system indicators
  year = data.single[[1]]$year.id,
  T = length(unique(data.single[[1]]$year.id)),
  # individual level variables
  I = ncol(select(data.single[[1]], -c(agg.democ, high.democ, ccode, year, cntry.yr.id, year.id))),
  X = select(data.single[[1]], -c(agg.democ, high.democ, ccode, year, cntry.yr.id, year.id))
)



# Limited redundant calculation gains w/ state variables
nrow(stan.data.single$X)
nrow(distinct(stan.data.single$X))

# compile cmdstan model
stan.model.cmd.covar <- cmdstan_model("data/ml-model-wvs-reduce-covar.stan",
                                    cpp_options = list(stan_threads = TRUE))

# run model- check code
stan.model.cmd.covar$print()
stan.model.cmd.covar$exe_file()


# quick check with variational approx: 
vb.wvs.covar <- stan.model.cmd.covar$variational(data = stan.data.single, seed = 123, 
                                                 output_samples = 1000,
                                                 threads = 2)
vb.wvs.covar$cmdstan_summary()
vb.wvs.covar$cmdstan_diagnose()
rm(vb.wvs.covar) # remove 

# stan model fit: takes ~7 days
# system.time(
#   fit.wvs.covar <- stan.model.cmd.covar$sample(
#     data = stan.data.single,
#     seed = 12,
#     iter_warmup = 1000,
#     iter_sampling = 1000,
#     chains = 4,
#     parallel_chains = 4,
#     threads_per_chain = 2,
#     refresh = 200,
#     max_treedepth = 20,
#     adapt_delta = .9
#   )
# )
# # save outside of workspace
# fit.wvs.covar$save_object(file = "data/fit-wvs-covar.RDS")


# diagnose
# fit.wvs.covar$cmdstan_diagnose()
# fit.wvs.covar$cmdstan_summary()
# 
# diagnostics_df <- as_draws_df(fit.wvs.covar$sampler_diagnostics())
# print(diagnostics_df)

# autocorrelation
mcmc_acf(vb.wvs.covar$draws("alpha"))

mcmc_pairs(vb.wvs.covar$draws(),
           pars = c("alpha", "sigma_state", 
                    "sigma"),
           off_diag_args = list(size = 0.75))

# summarize parameters
vb.wvs.covar$summary("alpha")
mcmc_hist(vb.wvs.covar$draws("alpha"))

# individual-level regressors
vb.wvs.covar$summary("beta")[1:9, ] # indiv
vb.wvs.covar$summary("beta")[10:19, ] # state
vb.wvs.covar$summary("beta")[20:29, ] # system
mcmc_intervals(vb.wvs.covar$draws("beta"))


# year-level regressors plot
mcmc_intervals(vb.wvs.covar$draws(), pars = vars(param_range("beta", c(20:29))),
               prob = .9, point_est = "median") + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    labels = lambda.labs) +
  ggtitle("Year Level: US Success")

