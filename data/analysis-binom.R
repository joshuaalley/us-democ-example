# Joshua Alley and John Owen
# Binomial model of democratic support in WVS

# Model proportion of respondents in each state that support democracy


### data prep
# count by state-year group
imputed.wvs.sum <- lapply(imputed.data.wvs, function(x)
                          x %>% group_by(cntry.yr.id) %>% 
                          summarize(high.democ.sum = sum(high.democ),
                                    avg.interest.pol = mean(interest.pol),
                                    avg.country.aim = mean(country.aim),
                                    avg.left.right = mean(left.right),
                                    avg.gov.conf = mean(gov.conf),
                                    avg.rate.pol.sys = mean(rate.pol.sys),
                                    avg.nationalism = mean(nationalism),
                                    avg.financial.sat = mean(financial.sat),
                                    avg.resp.auth = mean(resp.auth),
                                    n.res = n(),
                                    cntry.id = mean(cntry.id),
                                    cntry.yr.id = mean(cntry.yr.id),
                                    year.id = mean(year.id),
                                    ccode = mean(ccode),
                                    year = mean(year),
                                    .groups = "keep"
                                    ) %>%
                            mutate(
                              constant = 1,
                              region = ifelse(ccode < 200, 1, # Americas 
                                ifelse(ccode %in% 200:400, 2, # Europe
                            ifelse(ccode > 400 & ccode < 600, 3, # subs Africa
                            ifelse(ccode >= 600 & ccode < 700, 4, # MENA
                            ifelse(ccode > 700, 5, 0))))) # Asia
                          ) %>% 
                          left_join(democ.sum)
                          ) # end lapply
glimpse(imputed.wvs.sum[[1]])


# summary table: 1st imputed WVS dataset 
wvs.sum <- bind_rows(imputed.wvs.sum) %>%
  ungroup() %>%
  select(-c(cntry.yr.id, ccode, year, cntry.id, year.id, region,
            mean.democ, sd.democ, consol.democ, constant))
colnames(wvs.sum) <- c("Sum: High Democ. Support",
                       "Avg: Political Interest", "Avg: Country Aim", "Avg: Left or Right",
                       "Avg: Government Confidence", "Avg: Rate Political System",
                       "Avg: Nationalism", "Avg: Financial Satisfaction",
                       "Avg: Respect Authority", "Number of Respondents")
datasummary_skim(wvs.sum, fmt = "%.2f",
                 title = "Individual Level Variables",
                 histogram = FALSE,
                 output = "appendix/wvs-vars.tex")



### plot the raw data 
imputed.dem.prop <- bind_rows(imputed.wvs.sum) %>%
                     select(high.democ.sum, n.res,
                            cntry.yr.id, year.id) %>%
                     mutate(high.democ.prop = high.democ.sum / n.res) %>%
                     left_join(select(imputed.state.yr[[1]],
                                      ccode, year, cntry.yr.id)) %>%
                     left_join(filter(us.data.five,
                                      year %in% unique(state.year.data$year)) %>%
                                 mutate(
                                   # add presidential partisanship
                                   rep_pres = ifelse((year >= 1981 & year <= 1992) | # Reagan/Bush
                                                       (year >= 2001 & year <= 2008) | # HW Bush
                                                       (year >= 2017), # Trump
                                                     1, 0),
                                   # and cold war
                                   post_cold_war = ifelse(year >= 1989 & 
                                                            year <= 1994, 1, 0),
                                   constant = 1
                                 ) %>% # add protest data
                                 left_join(gdelt.protests))

# plot the raw data
ggplot(imputed.dem.prop, aes(x = high.democ.prop)) + 
  geom_histogram()
# plot the raw data over time
ggplot(imputed.dem.prop, aes(x = year, y = high.democ.prop)) + 
  geom_count(alpha = .5)
# democracy
ggplot(imputed.dem.prop, aes(x = v2x_libdem_VDEM, y = high.democ.prop)) +
     geom_count(alpha = .5) +
     geom_smooth(method = "loess")
# human rights
ggplot(imputed.dem.prop, aes(x = fariss_hr, y = high.democ.prop)) +
  geom_count(alpha = .5) +
  geom_smooth(method = "loess")
# economic growth
ggplot(imputed.dem.prop, aes(x = growth_WDI_PW, y = high.democ.prop)) +
  geom_count(alpha = .5) +
  geom_smooth(method = "loess")


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
mcmc_trace(draws.binom, pars = vars(param_range("lambda", c(1:12))))
ggsave("appendix/lambda-trace.png", height = 6, width = 8)

# look at dispersion and other key params
mcmc_intervals(draws.binom, pars = c("sigma", "sigma_state", "sigma_year"),
               prob = .9, point_est = "median")

# vectors of parameter names 
colnames(stan.data.binom$G) # omit constant in year-level reg
lambda.labs = c("US GDP Growth", "US Democracy", "US Human Rights",
                "US Protests", "US GINI", "Clinton", "W. Bush", 
                "Obama", "Trump","Chinese Growth",
                "US Intervention")
colnames(stan.data.binom$Z)
gamma.labs = c("GDP per Capita", "GDP Growth", "Information Flow",
               "Social Globalization", "Bank Crisis",
               "Liberal Democracy", "Conflict Battle Deaths", "Human Rights",
               "Inequality")
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
             ggtitle("Year Level: US Success")
plot.us
colnames(stan.data.binom$G)
mean(draws.binom$`lambda[10]` < 0)
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
ggsave("figures/ml-res.png", ml.res, height = 6, width = 8)


# plot theta parameters 
mcmc_intervals(draws.binom, pars = vars(param_range("theta", c(1:268))),
               prob = .9, point_est = "median")
# draw theta pars, get quantiles
theta.pars <- t(select(draws.binom, starts_with("theta")) %>% 
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

# plot theta over time with loess 
filter(theta.pars, year >= 1994) %>%
ggplot(aes(x = year, y = median)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=position_jitter(width=0.5)) +
  geom_smooth() +
  labs(x = "Year", y = "Estimated Probability of High Democratic Support") +
  ggtitle("Estimated Support for Democracy: 1994-2018")
ggsave("figures/theta-only.png", height = 6, width = 8)

# plot theta pars
filter(theta.pars, year >= 1994) %>%
ggplot(aes(x = year, y = median,
           color = president)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), 
                  position=position_jitter(width=0.5)) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Year", y = "Probability of High Democratic Support")

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
              labs(x = "Year", y = "Probability of High Democratic Support",
              color = "President",
              title = "Estimated Probability of High Democratic Support: 1994-2018")
pres.theta


# combine
grid.arrange(pres.theta, pres.avg, nrow = 2)
theta.est <- arrangeGrob(pres.theta, pres.avg, nrow = 2)
ggsave("figures/theta-est.png", theta.est, height = 6, width = 8)



