# Joshua Alley and John Owen
# Varying slopes by state: 
# analyze states with multiple WVS waves


# start with determining number of waves
# group-level variables
state.vars  <- select(imputed.state.yr[[1]], ccode, year) %>% 
  group_by(ccode) %>%
  summarize(
    ccode = mean(ccode),
    nwaves = n()
  ) %>%
  mutate( # regional dummies with americas as base
    constant = 1,
    europe = ifelse(ccode >= 200 & ccode < 400, 1, 0),
    africa = ifelse(ccode > 400 & ccode < 600, 1, 0),
    mena = ifelse(ccode >= 600 & ccode < 700, 1, 0),
    asia = ifelse(ccode > 700, 1, 0)
  )

state.mwaves <- filter(state.vars, nwaves > 1)

# cut down country-specific data to match
imputed.wvs.mwave <- imputed.wvs.sum[stan.data.binom.draw]

imputed.wvs.mwave <- lapply(imputed.wvs.mwave, function(x)
  filter(x, ccode %in% state.mwaves$ccode) %>%
  group_by(ccode) %>%
  mutate(cntry.id = cur_group_id()) %>%
  group_by(year) %>%
  mutate(year.id = cur_group_id()))

# state-level variables
imputed.state.yr.mwave <- imputed.state.yr.final[stan.data.binom.draw]

imputed.state.yr.mwave <- lapply(imputed.state.yr.mwave, function(x)
  filter(x, ccode %in% state.mwaves$ccode))

# year level variables
us.data.long.vars<- left_join(select(imputed.state.yr.mwave[[1]], ccode, year), 
                                 cbind.data.frame(us.data.final, filter(us.data.five,
                                           year %in% unique(state.year.data$year))%>%
                                                    select(year))
)%>%
  select(-c(ccode, year))
glimpse(us.data.long.vars)


# compile model code
stan.bin.var.state <- cmdstan_model("data/ml-model-binom-var-state.stan")
# check code
stan.bin.var.state$print()
stan.bin.var.state$exe_file()


# loop over 5 imputed datasets and fit the model to each
draws.binom.vars <- vector(mode = "list", length = length(imputed.wvs.mwave))
for(i in 1:length(imputed.wvs.mwave)){
  
  # set up stan data 
  # create data list
  stan.data.binom.vars <- list(
    N = nrow(imputed.wvs.mwave[[i]]),
    n_res = imputed.wvs.mwave[[i]]$n.res,
    y = imputed.wvs.mwave[[i]]$high.democ.sum,
    # state-year indicators
    state = imputed.wvs.mwave[[i]]$cntry.id,
    S = length(unique(imputed.wvs.mwave[[i]]$cntry.id)),
    # year/system indicators
    year = imputed.wvs.mwave[[i]]$year.id,
    T = length(unique(imputed.wvs.mwave[[i]]$year.id)),
    # individual level variables
    I = ncol(imputed.wvs.mwave[[i]][, 3:10]),
    X = imputed.wvs.mwave[[i]][, 3:10],
    # state level variables
    J = ncol(imputed.state.yr.mwave[[i]][, 3:11]),
    Z = imputed.state.yr.mwave[[i]][, 3:11],
    # year/US/system level vars
    L = ncol(us.data.long.vars),
    G = us.data.long.vars
  )
  
  # stan model fit
  system.time(
    fit.wvs.bin.vars <- stan.bin.var.state$sample(
      data = stan.data.binom.vars,
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
  fit.wvs.bin.vars$cmdstan_diagnose()
  
  draws.binom.vars[[i]] <- as_draws_df(fit.wvs.bin.vars$draws()) 
  
}

# combine draws into a full posterior
draws.binom.vars <- bind_rows(draws.binom.vars)
saveRDS(draws.binom.vars, "data/draws-all-vars.RDS")
draws.binom.vars <- select(draws.binom.vars, 
                           starts_with(c("lambda")))




# illegible intervals: work with medians and facets
lambda.vars <- draws.binom.vars %>% 
  select(starts_with("lambda")) %>% 
  summarise(across(everything(), list( ~quantile(., probs = c(0.05, 0.5, .95)))))
lambda.vars <- as.data.frame(t(lambda.vars))
colnames(lambda.vars) <- c("lower", "median", "upper")
# add state and parameter labels
lambda.vars$cntry.id <- rep(seq(from = 1, to = 72, by = 1), each = 11)
lambda.vars$param <- c("State Intercept", "US GDP Growth", "US Democracy", "US Human Rights",
                      "US Protests", "US GINI", "Republican Pres", "Trump Pres",
                      "Post Cold War", "Chinese Growth",
                      "US War Success")
# order labels as factor for plotting
lambda.vars$param <- factor(lambda.vars$param, levels = unique(lambda.vars$param))
# add ccode and cname
lambda.vars <- left_join(lambda.vars, unique(select(imputed.wvs.mwave[[1]], cntry.id, ccode)))
lambda.vars$cname <- countrycode(lambda.vars$ccode, origin = "cown", destination = "country.name")
lambda.vars$region <- countrycode(lambda.vars$ccode, origin = "cown", destination = "region")
# clean up names 
lambda.vars$cname[lambda.vars$ccode == 714] <- "Hong Kong"
lambda.vars$region[lambda.vars$ccode == 714] <- "East Asia & Pacific"
# combine the Americas
lambda.vars$region[lambda.vars$region == "North America"] <- "Americas"
lambda.vars$region[lambda.vars$region == "Latin America & Caribbean"] <- "Americas"
# combine the Asian regions
lambda.vars$region[lambda.vars$region == "South Asia"] <- "Asia"
lambda.vars$region[lambda.vars$region == "East Asia & Pacific"] <- "Asia"

# plot by parameter 
ggplot(lambda.vars, aes(x = median, y = param)) +
  facet_grid(rows = vars(region)) +
  geom_vline(xintercept = 0) +
  geom_point() +
  labs(x = "Country Code", y = "Posterior Median Slope") 
# alternative formulation
ggplot(lambda.vars, aes(y = median, x = factor(ccode),
                       color = region)) +
  facet_wrap(~ param) +
  geom_hline(yintercept = 0) +
  geom_point() +
  labs(x = "Country Code", y = "Posterior Median Slope",
       color = "Region") +
  theme(axis.text.x = element_blank())

ggplot(lambda.vars, aes(x = median, y = factor(ccode))) +
  facet_grid(rows = vars(region), cols = vars(param),
             scales = "free") +
  geom_vline(xintercept = 0) +
  geom_point() +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = .1) +
  labs(y = "Country Code", x = "Posterior Median Slope") +
  theme(axis.text.y = element_blank())
ggsave("figures/vars-state-region.png", height = 9, width = 12)

# plot by region
vars.plots <- vector(mode = "list", length = length(unique(lambda.vars$region)))
for(i in 1:length(unique(lambda.vars$region))){
  plot <- filter(lambda.vars, region == unique(lambda.vars$region)[i]) %>%
    ggplot(aes(x = median, y = param)) +
    facet_wrap(~ cname) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin = lower, xmax = upper),
                   height = .1) +
    geom_point() +
    labs(x = "Posterior Median and 90% Interval",
         y = "Parameter") +
    ggtitle(unique(lambda.vars$region)[i])
  print(plot)
  ggsave(paste0("figures/vars-", unique(lambda.vars$region)[i], 
                ".png"), height = 8, width = 7)
  vars.plots[[i]] <- plot
}



