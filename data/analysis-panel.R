# Joshua Alley and John Owen
# Democratic Mood analysis 


# us data for panel 
us.data.panel <- select(us.data.five, # key leadership variables
                         year, war_outcome, growth_WDI_PW,
                         v2x_libdem_VDEM, fariss_hr, 
                         Clinton, Obama, Trump, W.Bush) %>% # add protest data
                    left_join(gdelt.protests) %>%
  mutate(
    # add presidential partisanship
    rep_pres = ifelse((year >= 1981 & year <= 1992) | # Reagan/Bush
                        (year >= 2001 & year <= 2008),  # HW Bush
                      1, 0),
    # and cold war
    post_cold_war = ifelse(year >= 1989 &
                             year <= 2000, 1, 0),
  ) %>%
  select(-countryname) %>% 
  rename(
    us_growth_WDI_PW = growth_WDI_PW,
    us_v2x_libdem_VDEM = v2x_libdem_VDEM,
    us_fariss_hr = fariss_hr,
    us_perc.protest = perc.protest,
    us_war_outcome = war_outcome
  )
glimpse(us.data.panel)

# cl data with us
cl.data.us <- left_join(cl.data, us.data.panel) %>% 
                group_by(ccode) %>% 
                mutate( # lagged outcome 
                 lag_SupDem = lag(SupDem),
                 change_SupDem = SupDem - lag_SupDem,
                 # regional indicators
                 region = ifelse(ccode < 200, 1, # Americas 
                                 ifelse(ccode %in% 200:400, 2, # Europe
                                        ifelse(ccode > 400 & ccode < 600, 3, # subs Africa
                                               ifelse(ccode >= 600 & ccode < 700, 4, # MENA
                                                      ifelse(ccode > 700, 5, 0))))), # Asia
                ) %>% 
               filter(year >= 1995 & ccode > 2) # match WVS analysis
table(cl.data.us$year)
glimpse(cl.data.us)

# rescale by 2sd 
cl.data.us[, 5:(ncol(cl.data.us) - 3)] <- lapply(cl.data.us[, 5:(ncol(cl.data.us) - 3)],
                                                 function(x)
                                                 arm::rescale(x, 
                                                      binary.inputs = "0/1"))

# outcome
ggplot(cl.data.us, aes(x = SupDem)) + geom_histogram()
summary(cl.data.us$SupDem)

ggplot(cl.data.us, aes(x = year, y = SupDem)) +
  geom_point() +
  geom_smooth()

# lines by country
ggplot(cl.data.us, aes(x = year, y = SupDem,
                       group = ccode,
                       color = ccode)) +
  geom_line() +
  geom_smooth()

ggplot(cl.data.us, aes(x = year, y = SupDem)) +
  facet_wrap(~ ccode) +
  geom_line()


# regional variation
cl.data.reg <- cl.data.us %>%
                group_by(year, region) %>%
                summarize(
                  n = n(),
                  mean.supd = mean(SupDem, na.rm = TRUE),
                  sd.supd = sd(SupDem, na.rm = TRUE),
                  .groups = "keep"
                )
cl.data.reg$region <- recode(cl.data.reg$region,
                             `1` = "Americas", 
                               `2` = "Europe",
                               `3` = "Sub-Saharan Africa",
                                `4` = "Middle East and North Africa",
                                `5` = "Asia")

# plot by region
# mean 
ggplot(cl.data.reg, aes(x = year, y = mean.supd)) +
  facet_wrap(~ region) +
  geom_point() +
  geom_line()
ggsave("appendix/regional-trends.png", height = 6, width = 8)

# sd
ggplot(cl.data.reg, aes(x = year, y = sd.supd)) +
  facet_wrap(~ region) +
  geom_point() +
  geom_line()



### Simple models ### 
# unbalanced panel- simple panel model: LDV suggests unit root
cl.ols.ldv <- brm(SupDem ~ lag_SupDem + year +
              Trump + us_perc.protest + us_fariss_hr +
               us_war_outcome + us_growth_WDI_PW +
               us_v2x_libdem_VDEM + us.aid +
               gini_disp + fariss_hr + v2x_libdem_VDEM +
               gdppc_WDI_PW + bkcrisis_GFD,
              prior = c(set_prior("normal(0, 1)", class = "b")),
              backend = "cmdstanr",
              cores = 4,
              family = gaussian(link = "identity"),
             data = cl.data.us)
summary(cl.ols.ldv)


# model in changes
ggplot(cl.data.us, aes(x = change_SupDem)) + geom_histogram()
summary(cl.data.us$change_SupDem)

ggplot(cl.data.us, aes(x = year, y = change_SupDem)) +
  facet_wrap(~ ccode) +
  geom_line()

ggplot(cl.data.us, aes(x = year, y = change_SupDem)) +
  geom_point() +
  geom_smooth()

# Robust reg w/ year smoothed
cl.ols.chg <- brm(change_SupDem ~ 
                   Trump + Obama + W.Bush + 
                   us_perc.protest + us_fariss_hr +
                   us_war_outcome + us_growth_WDI_PW +
                   us_v2x_libdem_VDEM + us.aid +
                   gini_disp + fariss_hr + v2x_libdem_VDEM +
                   gdppc_WDI_PW + bkcrisis_GFD +
                   s(year),
                  prior = c(set_prior("normal(0, .25)", class = "b")),
                  backend = "cmdstanr",
                  cores = 4,
                  control = list(adapt_delta = .99),
                  family = student(),
                 data = cl.data.us)
summary(cl.ols.chg)


# add country RE 
cl.pres.fe <- brm(change_SupDem ~ year +
                  Trump + Obama + W.Bush +
                  us_perc.protest + us_fariss_hr +
                  us_war_outcome + us_growth_WDI_PW +
                  us_v2x_libdem_VDEM + us.aid +
                  gini_disp + fariss_hr + v2x_libdem_VDEM +
                  gdppc_WDI_PW + bkcrisis_GFD +
                  (1 | ccode),
                 prior = c(set_prior("normal(0, .25)", class = "b"),
                            set_prior("normal(0, .1)", class = "sd")),
                 backend = "cmdstanr",
                 family = student(),
                 cores = 4,
                data = cl.data.us)
summary(cl.pres.fe)


# Robust regression
# use complete cases for simple RLM w/ FE
cl.data.us.comp <- drop_na(cl.data.us, 
                           change_SupDem, 
                             Trump, Obama, W.Bush, 
                             us_perc.protest, us_fariss_hr,
                             us_war_outcome, us_growth_WDI_PW,
                             us_v2x_libdem_VDEM, us.aid,
                             gini_disp, fariss_hr, v2x_libdem_VDEM,
                             gdppc_WDI_PW, bkcrisis_GFD)
# OLS, no FE 
cl.rlm.chg <- MASS::rlm(change_SupDem ~ 
                   Trump + Obama + W.Bush + 
                   us_perc.protest + us_fariss_hr +
                   us_war_outcome + us_growth_WDI_PW +
                   us_v2x_libdem_VDEM + us.aid +
                   gini_disp + fariss_hr + v2x_libdem_VDEM +
                   gdppc_WDI_PW + bkcrisis_GFD,
                 data = cl.data.us.comp)
summary(cl.rlm.chg)

# add country FE with rlm- weighted OLS
cl.rlm.fe <- lm(change_SupDem ~ 
                  Trump + Obama + W.Bush + 
                  us_perc.protest + us_fariss_hr +
                  us_war_outcome + us_growth_WDI_PW +
                  us_v2x_libdem_VDEM + us.aid +
                  gini_disp + fariss_hr + v2x_libdem_VDEM +
                  gdppc_WDI_PW + bkcrisis_GFD +
                  factor(ccode),
                weights = cl.rlm.chg$w,
                data = cl.data.us.comp)
summary(cl.rlm.fe)

