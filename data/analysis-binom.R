# Joshua Alley and John Owen
# Binomial model of democratic support in WVS

# Model proportion of respondents in each state that support democracy





### Fit multilevel model

# compile cmdstan model
stan.model.cmd.bin <- cmdstan_model("data/ml-model-beta-binom.stan")

# run model- check code
stan.model.cmd.bin$print()
stan.model.cmd.bin$exe_file()


### Loop over 5 imputed datasets
# define list of draws
draws.binom <- vector(mode = "list", (length = 5))
# data to draw from imputation
stan.data.binom.draw <- sample(length(imputed.wvs.sum), 
                               size = 5, replace = FALSE)

# loop over five imputed datasets and fit the model to each
for(i in 1:length(draws.binom)){
  
 draw = stan.data.binom.draw[i]  
  # set up stan data 
  # create data list
stan.data.binom <- list(
    N = nrow(imputed.wvs.sum[[draw]]),
    n_res = imputed.wvs.sum[[draw]]$n.res,
    y = imputed.wvs.sum[[draw]]$high.democ.sum,
    # state-year indicators
    state = imputed.wvs.sum[[draw]]$cntry.id,
    S = length(unique(imputed.wvs.sum[[draw]]$cntry.id)),
    # year/system indicators
    year = imputed.wvs.sum[[draw]]$year.id,
    T = length(unique(imputed.wvs.sum[[draw]]$year.id)),
    # individual level variables
    I = ncol(imputed.wvs.sum[[draw]][, 3:10]),
    X = imputed.wvs.sum[[draw]][, 3:10],
    # state level variables
    J = ncol(imputed.state.yr.final[[draw]][, 3:11]),
    Z = imputed.state.yr.final[[draw]][, 3:11],
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
 
# print diagnostics
fit.wvs.bin$cmdstan_diagnose()

draws.binom[[i]] <- as_draws_df(fit.wvs.bin$draws()) 
  
}

# combine draws into a full posterior
draws.binom <- bind_rows(draws.binom)

# diagnostics
mcmc_trace(draws.binom, pars = vars(param_range("lambda", c(1:10))))

# look at dispersion and other key params
mcmc_intervals(draws.binom, pars = c("sigma", "sigma_state", "sigma_year"),
               prob = .9, point_est = "median")

# vectors of parameter names 
colnames(stan.data.binom$G) # omit constant in year-level reg
lambda.labs = c("US GDP Growth", "US Democracy", "US Human Rights",
                "US Protests", "US GINI", "Repub. Pres", "Trump","Chinese Growth",
                "US Intervention")
colnames(stan.data.binom$Z)
gamma.labs = c("GDP per Capita", "GDP Growth", "Information Flow",
               "Social Globalization", "Bank Crisis",
               "Liberal Democracy", "Conflict Battle Deaths", "Human Rights",
               "Inequality", "U.S. Aid")
colnames(stan.data.binom$X)
beta.labs = c("Political Interest", "Country Aim", "Left or Right",
               "Government Confidence", "Rate Political System",
               "Nationalism", "Financial Satisfaction",
               "Respect Authority")

# summarize the draws
plot.us <- mcmc_intervals(draws.binom, pars = vars(param_range("lambda", c(2:11))),
               prob = .9, point_est = "median") + 
               geom_vline(xintercept = 0) +
               scale_y_discrete(
                labels = lambda.labs) +
             ggtitle("Year Level: US Performance")
plot.us
colnames(stan.data.binom$G)
mean(draws.binom$`lambda[8]` < 0)
mcmc_intervals(draws.binom, pars = vars(param_range("alpha_year", c(1:28))))
# state-year level 
plot.state <- mcmc_intervals(draws.binom, regex_pars = "gamma",
                          prob = .9, point_est = "median") +
                 geom_vline(xintercept = 0) +
                 scale_y_discrete(
                      labels = gamma.labs) +
                 ggtitle("State Level")
plot.state
mcmc_intervals(draws.binom, pars = vars(param_range("alpha_state", c(1:99))))
# individual level
plot.indiv <- mcmc_intervals(draws.binom, regex_pars = "beta",
               prob = .9, point_est = "median") + 
              geom_vline(xintercept = 0) +
              scale_y_discrete(
               labels = beta.labs) +
              ggtitle("Individual Level")
plot.indiv


# combine plots
# all in one
grid.arrange(plot.indiv, plot.state, plot.us,
             layout_matrix = rbind(c(1, 2),
                                   c(3, 3))
)

ml.res <- arrangeGrob(plot.indiv, plot.state, plot.us,
                      layout_matrix = rbind(c(1, 2),
                                            c(3, 3))
)



