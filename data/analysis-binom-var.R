# Joshua Alley and John Owen
# Binomial Model with varying slopes by region and democracy

### varying slopes by region
# compile cmdstan model
stan.model.bin.var <- cmdstan_model("data/ml-model-wvs-binom-var.stan")

# run model- check code
stan.model.bin.var$print()
stan.model.bin.var$exe_file()

# us data by year 
us.data.single.long <- left_join(select(imputed.state.yr[[1]], ccode, year), 
                                 cbind.data.frame(us.data.final, filter(us.data.five,
                                                                        year %in% unique(state.year.data$year))%>%
                                                    select(year))
)%>%
  select(-c(ccode, year))
glimpse(us.data.single.long)


### Loop over 5 imputed datasets
# define list of draws
draws.binom.var <- vector(mode = "list", (length = 5))
# same data numbers drawn (stan.data.binom.draw)

# loop over five imputed datasets and fit the model to each
system.time(
  for(i in 1:length(draws.binom.var)){
    
    draw = stan.data.binom.draw[i]  
    # set up stan data 
    # create data list
    stan.data.binom.var <- list(
      N = nrow(imputed.wvs.sum[[draw]]),
      n_res = imputed.wvs.sum[[draw]]$n.res,
      y = imputed.wvs.sum[[draw]]$high.democ.sum,
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
      L = ncol(us.data.single.long),
      G = us.data.single.long,
      # regional indicators
      R = length(unique(imputed.wvs.sum[[1]]$region)),
      group = imputed.wvs.sum[[1]]$region
    )
    
    # stan model fit
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
    
    # print diagnostics
    fit.wvs.bin.var$cmdstan_diagnose()
    
    draws.binom.var[[i]] <- as_draws_df(fit.wvs.bin.var$draws()) 
    
  }
)

# combine draws into a full posterior
saveRDS(draws.binom.var, "data/draws-all-var.RDS")
# keep only year-level vars in workspace
draws.binom.var <- select(bind_rows(draws.binom.var), starts_with("lambda"))


# illegible intervals: work with medians and facets
lambda.var <- draws.binom.var %>% 
  summarise(across(everything(), list( ~quantile(., probs = c(0.05, 0.5, .95)))))
lambda.var <- as.data.frame(t(lambda.var))
colnames(lambda.var) <- c("lower", "median", "upper")
# add state and parameter labels
lambda.var$region <- rep(c("Americas", "Europe", "Sub-Saharan Africa",
                           "Middle East and North Africa",
                           "Asia"), each = 12)
lambda.var$param <- c("Region Intercept", lambda.labs)
# order labels as factor for plotting
lambda.var$param <- factor(lambda.var$param, levels = unique(lambda.var$param))

# plot by parameter 
ggplot(lambda.var, aes(x = median, y = param)) +
  facet_grid(cols = vars(region)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = .4) +
  labs(y = "Variable", x = "Posterior Median Slope") 
ggsave("figures/vars-region.png", height = 8, width = 10)


# alternative formulation
ggplot(lambda.var, aes(y = median, x = factor(region),
                       color = region)) +
  facet_wrap(~ param) +
  geom_hline(yintercept = 0) +
  geom_point() +
  labs(x = "Country Code", y = "Posterior Median Slope",
       color = "Region") +
  theme(axis.text.x = element_blank())

# plot by region
for(i in 1:length(unique(lambda.var$region))){
  plot <- filter(lambda.var, region == unique(lambda.var$region)[i]) %>%
    ggplot(aes(x = median, y = param)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = .1) +
    geom_point() +
    labs(x = "Posterior Median and 90% Interval",
         y = "Parameter") +
    ggtitle(unique(lambda.var$region)[i])
  print(plot)
}



### Analysis by consolidated democ 
### Loop over 5 imputed datasets
# define list of draws
draws.binom.democ <- vector(mode = "list", (length = 5))
# same data numbers drawn (stan.data.binom.draw)

# loop over five imputed datasets and fit the model to each
system.time(
  for(i in 1:length(draws.binom.democ)){
    
    draw = stan.data.binom.draw[i]  
    # set up stan data 
    # create data list
    stan.data.binom.var <- list(
      N = nrow(imputed.wvs.sum[[draw]]),
      n_res = imputed.wvs.sum[[draw]]$n.res,
      y = imputed.wvs.sum[[draw]]$high.democ.sum,
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
      L = ncol(us.data.single.long),
      G = us.data.single.long,
      # regional indicators
      R = length(unique(imputed.wvs.sum[[1]]$consol.democ)),
      group = imputed.wvs.sum[[1]]$consol.democ
    )
    
    # stan model fit
    fit.wvs.bin.democ <- stan.model.bin.var$sample(
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
    
    # print diagnostics
    fit.wvs.bin.democ$cmdstan_diagnose()
    
    draws.binom.democ[[i]] <- as_draws_df(fit.wvs.bin.democ$draws()) 
    
  }
)

# combine draws into a full posterior
saveRDS(draws.binom.democ, "data/draws-all-democ.RDS")
# keep only year-level vars in workspace
draws.binom.democ <- select(bind_rows(draws.binom.democ), starts_with("lambda"))



# illegible intervals: work with medians and facets
lambda.democ <- draws.binom.democ %>% 
  summarise(across(everything(), list( ~quantile(., probs = c(0.05, 0.5, .95)))))
lambda.democ <- as.data.frame(t(lambda.democ))
colnames(lambda.democ) <- c("lower", "median", "upper")
# add state and parameter labels
lambda.democ$democ <- rep(c("Autocracy", "Middle", "Democracy"), each = 11)
lambda.democ$param <- c("Democracy Group Intercept", "US GDP Growth", "US Democracy", "US Human Rights",
                      "US Protests", "US GINI", "Republican Pres", "Trump Pres",
                      "Post Cold War", "Chinese Growth",
                      "US Intervention")
# order labels as factor for plotting
lambda.democ$param <- factor(lambda.democ$param, levels = unique(lambda.democ$param))

# plot by parameter 
ggplot(lambda.democ, aes(x = median, y = param)) +
  facet_grid(cols = vars(democ)) +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_errorbar(aes(xmin = lower, xmax = upper),
                width = .1) +
  labs(y = "Variable", x = "Posterior Median Slope") 
ggsave("figures/vars-democ.png", height = 8, width = 10)


# plot by democracy group
for(i in 1:length(unique(lambda.democ$democ))){
  plot <- filter(lambda.democ, democ == unique(lambda.democ$democ)[i]) %>%
    ggplot(aes(x = median, y = param)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = .1) +
    geom_point() +
    labs(x = "Posterior Median and 90% Interval",
         y = "Parameter") +
    ggtitle(unique(lambda.democ$democ)[i])
  print(plot)
}




### Varying individual level slopes by country 
# compile model code
stan.model.bin.indiv <- cmdstan_model("data/ml-model-vars-indiv.stan")

# empty list of draws
draws.binom.indiv <- vector(mode = "list", (length = 5))
# same data numbers drawn (stan.data.binom.draw)

# loop over five imputed datasets and fit the model to each
system.time(
  for(i in 1:length(draws.binom.democ)){
    
    draw = stan.data.binom.draw[i]  
    # set up stan data 
    # create data list
    stan.data.binom.indiv <- list(
      N = nrow(imputed.wvs.sum[[draw]]),
      n_res = imputed.wvs.sum[[draw]]$n.res,
      y = imputed.wvs.sum[[draw]]$high.democ.sum,
      # year/system indicators
      year = imputed.wvs.sum[[draw]]$year.id,
      T = length(unique(imputed.wvs.sum[[draw]]$year.id)),
      # individual level variables
      I = ncol(cbind.data.frame(imputed.wvs.sum[[1]]$constant,
                      imputed.wvs.sum[[draw]][, 3:10])),
      X = cbind.data.frame(imputed.wvs.sum[[1]]$constant, # add state intercepts
                           imputed.wvs.sum[[draw]][, 3:10]),
      # state level variables
      J = ncol(imputed.state.yr.final[[draw]][, 3:11]),
      Z = imputed.state.yr.final[[draw]][, 3:11],
      # year/US/system level vars
      L = ncol(us.data.final),
      G = us.data.final,
      # state-year indicators
      state = imputed.wvs.sum[[draw]]$cntry.id,
      S = length(unique(imputed.wvs.sum[[draw]]$cntry.id))
    )
    
    # stan model fit
    fit.wvs.bin.indiv <- stan.model.bin.indiv$sample(
      data = stan.data.binom.indiv,
      seed = 12,
      iter_warmup = 1000,
      iter_sampling = 1000,
      chains = 4,
      parallel_chains = 4,
      refresh = 200,
      max_treedepth = 20,
      adapt_delta = .98
    )
    
    # print diagnostics
    fit.wvs.bin.indiv$cmdstan_diagnose()
    
    draws.binom.indiv[[i]] <- as_draws_df(fit.wvs.bin.indiv$draws()) 
    
  }
)

# combine draws into a full posterior
saveRDS(draws.binom.indiv, "data/draws-all-indiv.RDS")
# keep only year-level vars in workspace
draws.binom.indiv <- select(bind_rows(draws.binom.indiv), starts_with("lambda"))


mcmc_intervals(draws.binom.indiv, pars = vars(param_range("lambda", c(2:11))),
               prob = .9, point_est = "median") + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    labels = lambda.labs) +
  ggtitle("Year Level: US Success")
ggsave("appendix/year-var-indiv.png", height = 6, width = 8)

# trump parameter is 94% negative
mean(draws.binom.indiv$`lambda[8]` < 0)
