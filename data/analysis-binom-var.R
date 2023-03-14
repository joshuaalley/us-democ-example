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
                                                    select(year))) %>%
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
      J = ncol(imputed.state.yr.final[[draw]][, 3:12]),
      Z = imputed.state.yr.final[[draw]][, 3:12],
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

draws.binom.var <- bind_rows(draws.binom.var)


# vectors of parameter names 
colnames(stan.data.binom.var$G) # omit constant in year-level reg
lambda.labs = c("US GDP Growth", "US Democracy", "US Human Rights",
                "US Protests", "US GINI", "Repub. Pres", "Trump","Chinese Growth",
                "US Intervention")
colnames(stan.data.binom.var$Z)
gamma.labs = c("GDP per Capita", "GDP Growth", "Information Flow",
               "Social Globalization", "Bank Crisis",
               "Liberal Democracy", "Conflict Battle Deaths", "Human Rights",
               "Inequality", "U.S. Aid")
colnames(stan.data.binom.var$X)
beta.labs = c("Political Interest", "Country Aim", "Left or Right",
              "Government Confidence", "Rate Political System",
              "Nationalism", "Financial Satisfaction",
              "Respect Authority")

# illegible intervals: work with medians and facets
lambda.var <- draws.binom.var %>% 
  select(starts_with("lambda")) %>%
  summarise(across(everything(), list( ~quantile(., probs = c(0.05, 0.5, .95)))))
lambda.var <- as.data.frame(t(lambda.var))
colnames(lambda.var) <- c("lower", "median", "upper")
# add state and parameter labels
lambda.var$region <- rep(c("Americas", "Europe", "Sub-Saharan Africa",
                           "Middle East and North Africa",
                           "Asia"), each = 10)
lambda.var$param <- c("Region Intercept", lambda.labs)
# order labels as factor for plotting
lambda.var$param <- factor(lambda.var$param, levels = unique(lambda.var$param))

# plot by parameter 
ggplot(lambda.var, aes(x = median, y = param)) +
  facet_grid(cols = vars(region)) +
  geom_vline(xintercept = 0) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = .5) +
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
    geom_pointrange(aes(xmin = lower, xmax = upper),
                   size = .6) +
    labs(x = "Posterior Median and 90% Interval",
         y = "Parameter") +
    ggtitle(unique(lambda.var$region)[i])
  print(plot)
}


# pull year intercepts
year.re <- draws.binom.var %>% 
  select(starts_with("alpha_year")) %>%
  summarise(across(everything(), list( ~quantile(., probs = c(0.05, 0.5, .95)))))

year.re <- as.data.frame(t(year.re[, 31:60]))
colnames(year.re) <- c("lower", "median", "upper")

# link to spec years 
year.re$year.id <- parse_number(row.names(year.re))

years <- imputed.wvs.sum[[2]] %>%
          group_by(year.id) %>%
          summarize(
            year = mean(year)
          ) 

year.re <- left_join(year.re, years) %>%
            drop_na()

# plot 
ggplot(year.re, aes(x = year, y = median)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) 


# plot theta parameters:
# draw theta pars, get quantiles
theta.pars <- t(select(draws.binom.var, starts_with("theta")) %>% 
                  summarise(across(everything(), list( ~quantile(., probs = c(0.05, 0.5, .95))))))
# add link function
theta.pars <- apply(theta.pars, 2, function(x) exp(x) / (1 + exp(x)))
theta.pars <- cbind.data.frame(theta.pars, imputed.wvs.sum[[1]]$year)
colnames(theta.pars) <- c("lower", "median", "upper", "year")
# create a presidential indicator
theta.pars$president <- factor(ifelse(theta.pars$year < 1993, "Reagan/Bush",
                                      ifelse(theta.pars$year >= 1993 & theta.pars$year <= 2000, "Clinton",
                                             ifelse(theta.pars$year >= 2001 & theta.pars$year < 2009, "Bush",
                                                    ifelse(theta.pars$year >= 2009 & theta.pars$year < 2017, "Obama",
                                                           "Trump")))))

# summarize 
theta.pars.sum <- theta.pars %>%
  group_by(president) %>%
  select(median, president, year) %>%
  summarize(
    median.prob = mean(median),
    se = sd(median) / sqrt(n()),
    year = mean(year)
  )

# add ccode and region
theta.pars <- theta.pars %>%
              mutate(
                ccode = imputed.state.yr[[1]]$ccode,
                region = ifelse(ccode < 200, "Americas", # Americas 
                          ifelse(ccode %in% 200:400, "Europe", # Europe
                           ifelse(ccode > 400 & ccode < 600, "Africa", # subs Africa
                            ifelse(ccode >= 600 & ccode < 700, "MENA", # MENA
                             ifelse(ccode > 700, "Asia", 0))))))





# plot theta over time with loess 
filter(theta.pars, year >= 1994) %>%
  ggplot(aes(x = year, y = median)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=position_jitter(width=0.5)) +
  geom_smooth() +
  labs(x = "Year", y = "Estimated Probability of High Democratic Support") +
  ggtitle("Estimated Support for Democracy: 1994-2018")

# plot theta pars
filter(theta.pars, year >= 1994) %>%
  ggplot(aes(x = year, y = median,
             color = president)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=position_jitter(width=0.5)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Year", y = "Probability of High Democratic Support")


# plot theta pars by region- president facet
filter(theta.pars, year >= 1994) %>%
  ggplot(aes(x = year, y = median,
             color = factor(president))) +
  facet_wrap(~ region) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=position_jitter(width=0.5),
                  alpha = .75) +
  scale_color_manual(values = wes_palette("Zissou1")) +
  labs(x = "Year", y = "Estimated Probability of High Democratic Support",
       color = "President")
ggsave("appendix/theta-reg.png", height = 6, width = 8)

# Average by president 
pres.avg <- filter(theta.pars.sum, year >= 1994) %>%
  ggplot(aes(x = year, y = median.prob,
             ymin = median.prob - 2*se,
             ymax = median.prob + 2*se,
             label = president)) +
  geom_pointrange() +
  geom_text(nudge_y = 0.09, size = 5) +
  labs(x = "Year", y = "Average Democractic Support",
       title = "Average Probability of High Democratic Support by President")
pres.avg

# for publication
pres.theta <- filter(theta.pars, year >= 1994) %>%
  ggplot(aes(x = year, y = median,
             color = factor(president))) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=position_jitter(width=0.5)) +
  scale_colour_grey() +
  #scale_color_manual(values = wes_palette("Zissou1")) +
  labs(x = "Year", y = "Probability of High Democratic Support",
       color = "President",
       title = "Estimated Probability of High Democratic Support: 1994-2018")
pres.theta


# combine
grid.arrange(pres.theta, pres.avg, nrow = 2)
theta.est <- arrangeGrob(pres.theta, pres.avg, nrow = 2)
ggsave("figures/theta-est.png", theta.est, height = 6, width = 8)



# state-year level 
plot.state.vars <- mcmc_intervals(as.data.frame(draws.binom.var), regex_pars = "gamma",
                             prob = .9, point_est = "median") +
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    labels = gamma.labs) +
  ggtitle("State Level")
plot.state.vars
# individual level
plot.indiv.vars <- mcmc_intervals(as.data.frame(draws.binom.var), regex_pars = "beta",
                             prob = .9, point_est = "median") + 
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    labels = beta.labs) +
  ggtitle("Individual Level")
plot.indiv.vars


# combine 
grid.arrange(plot.indiv.vars, plot.state.vars, nrow = 2)

other.levels.vars <- arrangeGrob(plot.indiv.vars, plot.state.vars, nrow = 2)
ggsave("appendix/other-levels-vars.png", other.levels.vars, height = 6, width = 8)


# plot year intercepts 
mcmc_intervals(as.data.frame(draws.binom.var), regex_pars = "alpha_year",
               prob = .9, point_est = "median")



### Analysis by consolidated democ ####
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
      adapt_delta = .98
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
lambda.democ$democ <- rep(c("Autocracy", "Unstable", "Democracy"), each = 12)
lambda.democ$param <- c("Democracy Group Intercept", "US GDP Growth", "US Democracy", "US Human Rights",
                        "US Protests", "US GINI", "Clinton", "W. Bush", 
                        "Obama", "Trump","Chinese Growth",
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
ggsave("appendix/democ-var-slopes.png", height = 8, width = 10)


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


