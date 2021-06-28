# Joshua Alley and John Owen
# Model attitudes towards democracy in Asia


# create dataset at one level
us.data.single <- filter(us.data.five,
                         year %in% unique(state.year.data$year)) %>%
 # add protest data
  left_join(gdelt.protests) %>% # select key
  select(year, growth_WDI_PW, perc.protest, v2x_libdem_VDEM,
         fariss_hr, gini_disp, 
         Clinton, W.Bush, Obama, Trump,
         chinese_growth, war_outcome) %>%
  rename(us_hr = fariss_hr,
         us_growth = growth_WDI_PW,
         us_ineq = gini_disp,
         us_vdem = v2x_libdem_VDEM)

us.data.single[, 2:12] <-  apply(us.data.single[, 2:12], 2, 
                                function(x) arm::rescale(x, binary.inputs = "0/1"))





# single WVS dataset
# look at asia data
wvs.asia <- filter(wvs, region == 5 &
                     (ccode != 704 & # only one year of Uzbekistan- creates collinearity
                        ccode != 714))
wvs[sample(nrow(wvs.asia), size = 20000), ] %>% 
  vis_miss()

wvs.asia <- left_join(wvs.asia, 
                      us.data.single)

# country codes 
ccode.asia <- as.numeric(unique(wvs.asia$ccode))

# state year variables- rescale first
state.year.asia <- filter(state.year.data, 
                          ccode %in% ccode.asia) %>%  
  select(ccode, year, cntry.yr.id,
         growth_WDI_PW, v2x_libdem_VDEM,
         bdeadbest_BD, us.aid)

# state.year.asia[, 4:7] <- apply( state.year.asia[, 4:7], 2,
#   function(x) arm::rescale(x, binary.inputs = "0/1"))
# join rescaled data
wvs.asia <- left_join(wvs.asia, state.year.asia)


# visualize missing
wvs.asia[sample(nrow(wvs.asia), size = 10000), ] %>% 
  vis_miss()

wvs.asia[wvs.asia$ccode == 710, ] %>% 
  vis_miss()

# number of unique years
for(i in 1:length(asia.models)){
  
  cntry.data <- filter(wvs.asia, ccode == ccode.asia[i])
  print(length(unique(cntry.data$year)))
}

# model without imputation
# list of models for each country
asia.models <- vector(mode = "list",
                      length(ccode.asia))

names(asia.models) <- ccode.asia

for(i in 1:length(asia.models)){
  
  cntry.data <- filter(wvs.asia, ccode == ccode.asia[i]) %>%
    mutate(
      agg.dem.fac = as.ordered(agg.democ + 1)
    )
  
  # ifelse to capture Trump years
  # cannot add other year variables- too little variation
  if(max(cntry.data$Trump, na.rm = TRUE) == 1 &
     length(unique(cntry.data$year > 1))){  
    asia.models[[i]] <- ordinal::clm(
      agg.dem.fac ~ 
        interest.pol + country.aim +
        gov.conf +
        nationalism + financial.sat + resp.auth +
        growth_WDI_PW + Trump, 
      #v2x_libdem_VDEM +
      #bdeadbest_BD + us.aid +
      #us_vdem +  us_hr + us_ineq +
      #us_growth + perc.protest  +
      #war_outcome + chinese_growth,
      data = cntry.data,
      link = "logit",
      threshold = "flexible"
    )
  }else{
    asia.models[[i]] <- cntry.data
    class(asia.models[[i]]) <- "data.frame" # to make filtering easier
  }
  
}

# tidy up the models
asia.models.fit <- asia.models[sapply(asia.models, 
                                      function(x) class(x) == "clm")]

# use broom to get coefs 
asia.models.fit <- lapply(asia.models.fit, function(x)
  broom::tidy(x))

# pull into a dataset 
asia.models.res <- bind_rows(asia.models.fit,
                             .id = "ccode") 
# rename variables and country names for plotting
asia.models.res$term[asia.models.res$term == "interest.pol"] <- 
  "Political Interest"
asia.models.res$term[asia.models.res$term == "country.aim"] <- 
  "Country Aim"
asia.models.res$term[asia.models.res$term == "financial.sat"] <- 
  "Financial Satisfaction"
asia.models.res$term[asia.models.res$term == "gov.conf"] <- 
  "Government Confidence"
asia.models.res$term[asia.models.res$term == "resp.auth"] <- 
  "Respect Authority"
asia.models.res$term[asia.models.res$term == "nationalism"] <- 
  "Nationalism"
asia.models.res$term[asia.models.res$term == "growth_WDI_PW"] <- 
  "GDP Growth"

asia.models.res$cname <- countrycode(asia.models.res$ccode,
                                     origin = "cown",
                                     destination = "country.name")

# plot results
ggplot(filter(asia.models.res, coef.type == "location"),
       aes(y = term, x = estimate)) +
  facet_wrap(~ cname, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = estimate - 1.96*std.error,
                      xmax = estimate + 1.96*std.error))
# superior formulation
ggplot(filter(asia.models.res, coef.type == "location"),
       aes(y = cname, x = estimate,
           color = cname)) +
  facet_wrap(~ term, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = estimate - 1.96*std.error,
                      xmax = estimate + 1.96*std.error)) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Coefficient Estimate",
       y = "Country",
       title = "Sources of Democratic Support in Separate Asian Countries",
       subtitle = "1980-2019") +
  theme(legend.position = "none") 
ggsave("appendix/asia-state-noimp.png", height = 6, width = 8)

# thresholds by country
ggplot(filter(asia.models.res, coef.type == "intercept"),
       aes(y = term, x = estimate)) +
  facet_wrap(~ cname, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = estimate - 1.96*std.error,
                      xmax = estimate + 1.96*std.error))



### with imputation
# list of data frames
imputed.wvs.asia  <- vector(mode = "list", length = length(imputed.data.wvs))
imputed.state.asia  <- vector(mode = "list", length = length(imputed.data.wvs))
for(i in 1:length(imputed.wvs.asia)){
  
  imputed.state.asia[[i]] <- imputed.state.yr[[i]] %>%
    mutate( 
    region = ifelse(ccode < 200, 1, # Americas 
                    ifelse(ccode %in% 200:400, 2, # Europe
                           ifelse(ccode > 400 & ccode < 600, 3, # subs Africa
                                  ifelse(ccode >= 600 & ccode < 700, 4, # MENA
                                         ifelse(ccode > 700, 5, 0))))) # Asia
  ) %>% 
    filter(region == 5 & 
             (ccode != 704 & # only one year of Uzbekistan- creates collinearity
                ccode != 714) # hong Kong missing everything
    ) %>%
    select(-cntry.yr.id)
  
  # rescale by 2sd 
  imputed.state.asia[[i]][, 3:12] <- 
      as.data.frame(apply(imputed.state.asia[[i]][, 3:12],
                      2, function(x) arm::rescale(x, binary.inputs = "0/1")))

  
  imputed.wvs.asia[[i]] <- left_join(imputed.data.wvs[[i]],
                                imputed.state.asia[[i]]) %>%
    filter(region == 5)
  imputed.wvs.asia[[i]] <- left_join(imputed.wvs.asia[[i]],
                                us.data.single) %>%
    filter(ccode %in% unique(asia.models.res$ccode)) # keep only usable Trump data
}

# take 5 at random
imputed.wvs.asia <- sample(imputed.wvs.asia, size = 5, replace = FALSE)
glimpse(imputed.wvs.asia[[1]])
rm(imputed.state.asia)


# ordinal model in brms
# add one to outcome so all positive integers

draws.asia <- vector(mode = "list", (length = 5))
ccode.asia.set <- as.numeric(unique(asia.models.res$ccode))

# length(draws.binom) is full, start with 1 dataset
system.time(
for(i in 1:length(draws.asia)){
  
  # list of models for each country
  country.models <- vector(mode = "list",
                           length(ccode.asia.set))
  
  # fit model to each country w/ adequate data
  for(c in 1:length(ccode.asia.set)){
    
  country.models[[c]] <- brm(
    (agg.democ + 1) ~ 
      interest.pol + country.aim +
      gov.conf +
      nationalism + financial.sat + resp.auth +
      growth_WDI_PW + Trump,
    data = filter(imputed.wvs.asia[[i]], ccode == ccode.asia.set[c]), 
    family = cumulative(link = "logit", threshold = "flexible"),
    iter = 2000,
    chains = 4, cores = 4, backend = "cmdstanr", threads = threading(2),
    control = list(max_treedepth = 15, adapt_delta = .85),
    prior = c(set_prior(prior = "normal(0, 2)", class = "Intercept"),
              set_prior(prior = "normal(0, 2)", class = "b"))
  )
  }
  draws.asia[[i]] <- country.models
} # 16 hours total
)

# combine draws into a full posterior
saveRDS(draws.asia, "data/draws-asia.RDS")

# combine models
draws.asia.comb <- vector(mode = "list", length = length(ccode.asia.set))
for(i in 1:length(draws.asia.comb)){
  draws.asia.comb[[i]] <- combine_models(
    draws.asia[[1]][[i]],
    draws.asia[[2]][[i]],
    draws.asia[[3]][[i]],
    draws.asia[[4]][[i]],
    draws.asia[[5]][[i]],
    check_data = FALSE
  )
}

names(draws.asia.comb) <- ccode.asia.set

# create parameter vector
asia.pars.state <- lapply(draws.asia.comb, function(x) 
                          as.data.frame(
                            fixef(x, probs = c(0.05, 0.95),)))

# pull into a dataset 
asia.pars.state <- bind_rows(asia.pars.state,
                             .id = "ccode") 
asia.pars.state$term <- rep(rownames(as.data.frame(fixef(draws.asia[[1]][[1]]))),
                            9)

# rename variables and country names for plotting
asia.pars.state$term[asia.pars.state$term == "interest.pol"] <- 
  "Political Interest"
asia.pars.state$term[asia.pars.state$term == "country.aim"] <- 
  "Country Aim"
asia.pars.state$term[asia.pars.state$term == "financial.sat"] <- 
  "Financial Satisfaction"
asia.pars.state$term[asia.pars.state$term == "gov.conf"] <- 
  "Government Confidence"
asia.pars.state$term[asia.pars.state$term == "resp.auth"] <- 
  "Respect Authority"
asia.pars.state$term[asia.pars.state$term == "nationalism"] <- 
  "Nationalism"
asia.pars.state$term[asia.pars.state$term == "growth_WDI_PW"] <- 
  "GDP Growth"

asia.pars.state$cname <- countrycode(asia.pars.state$ccode,
                                     origin = "cown",
                                     destination = "country.name")


# take a look at state-spec paramters
ggplot(filter(asia.pars.state,
              str_detect(term, "Intercept", negate = TRUE)),
       aes(y = cname, x = Estimate,
           color = cname)) +
  facet_wrap(~ term, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = Q5,
                      xmax = Q95)) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Coefficient Estimate",
       y = "Country",
       title = "Sources of Democratic Support in Asian Countries",
       subtitle = "1980-2019") +
  theme(legend.position = "none")
ggsave("appendix/asia-state-imp.png", height = 6, width = 8)


# Trump only
ggplot(filter(asia.pars.state,
              str_detect(term, "Trump")),
       aes(y = cname, x = Estimate)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = Q5,
                      xmax = Q95),
                  fatten = 2, size = 2) +
  scale_color_brewer(palette = "Paired") +
  labs(x = "Trump Coefficient Estimate",
       y = "Country",
       title = "Impact of Trump Presidency\non Democratic Support in Asian Countries") 
ggsave("appendix/asia-state-trump.png", height = 6, width = 8)


       
# plot intercepts
ggplot(filter(asia.pars.state,
              str_detect(term, "Intercept", negate = FALSE)),
       aes(y = term, x = Estimate,
           color = cname)) +
  facet_wrap(~ cname, scales = "free") +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = Q5,
                      xmax = Q95)) +
  theme(legend.position = "none")




### old code for an unsuccessful regional model w/ state split

# compile code 
# compile cmdstan model
stan.model.cmd.region <- cmdstan_model("data/ml-model-region.stan",
                                      cpp_options = list(stan_threads = TRUE))

# run model- check code
stan.model.cmd.region$print()
stan.model.cmd.region$exe_file()


# set up the STAN data
### Loop over 5 imputed datasets

# loop over five imputed datasets to create STAN data
draws.asia <- vector(mode = "list", (length = 5))
for(i in 1:length(draws.binom)){

  
  draw = i  
  # set up stan data 
  # create data list
  stan.data.asia <- list(
    N = nrow(imputed.wvs.asia[[draw]]),
    y = imputed.wvs.asia[[draw]]$agg.democ,
    nt = 9,
    # state-year indicators
    state = imputed.wvs.asia[[draw]]$cntry.id,
    S = length(unique(imputed.wvs.asia[[draw]]$cntry.id)),
    # # year/system indicators
    # year = imputed.wvs.asia[[draw]]$year.id,
    # T = length(unique(imputed.wvs.asia[[draw]]$year.id))
    # # individual and state level variables
    I = ncol(select(imputed.wvs.asia[[draw]],
                    -c(ccode, year, cntry.id, year.id, agg.democ))),
    X = as.matrix(select(imputed.wvs.asia[[draw]],
                    -c(ccode, year, cntry.id, year.id, agg.democ)))
    # # year/US/system level vars
    # L = ncol(us.data.asia),
    # Z = as.matrix(us.data.asia)
  )


# loop and fit models
  # stan model fit
  system.time(
    fit.wvs.asia <- stan.model.cmd.region$sample(
      data = stan.data.asia,
      seed = 12,
      iter_warmup = 1000,
      iter_sampling = 1000,
      chains = 4,
      parallel_chains = 4,
      threads_per_chain =2,
      refresh = 200,
      max_treedepth = 10,
      adapt_delta = .8
    )
  )
  
  # print diagnostics
  fit.wvs.asia$cmdstan_diagnose()
  
  draws.asia[[i]] <- as_draws_df(fit.wvs.asia$draws()) 
  
}

# pairs plots 
mcmc_pairs(fit.wvs.asia$draws())


# quick check with variational approx on last model in data
vb.wvs.asia <- stan.model.cmd.region$variational(data = stan.data.asia, seed = 123, output_samples = 1000,
                                                 threads = 2)
vb.wvs.asia$cmdstan_summary()
mcmc_intervals(vb.wvs.asia$draws("beta")) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    labels = lambda.labs) 
vb.wvs.asia$cmdstan_diagnose()


# PSIS sampling to check if it worked
vb.asia.draws <- data.frame(vb.wvs.asia$draws())
# log likelihood
ll.norm <- function(draws, data){
  mu = draws$alpha + 
    (data$x * draws[, which(grepl(colnames(vb.asia.draws), pattern = "beta"))]) 
    
  dnorm(data$y, 
        mean = mu, 
        sd = draws$sigma)  
}

loo.ap <-
  loo_approximate_posterior(
    x = ll.norm,
    draws = vb.asia.draws,
    data = stan.data.asia,
    log_p = vb.wvs.asia$draws("lp__"),
    log_g = vb.wvs.asia$draws("lp_approx__"),
    cores = 2
  )


