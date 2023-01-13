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
                             year <= 2000, 1, 0)
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
                 change_SupDem = SupDem - lag_SupDem
                ) %>% 
               filter(year >= 1995) # match WVS analysis
table(cl.data.us$year)
glimpse(cl.data.us)

# outcome
ggplot(cl.data.us, aes(x = SupDem)) + geom_histogram()
summary(cl.data.us$SupDem)

ggplot(cl.data.us, aes(x = year, y = SupDem)) +
  geom_point() +
  geom_smooth()

ggplot(cl.data.us, aes(x = year, y = SupDem)) +
  facet_wrap(~ ccode) +
  geom_line()

### Simple models ### 
# unbalanced panel- simple panel model: LDV suggests unit root
cl.ols.ldv <- lm(SupDem ~ lag_SupDem + 
              Trump + us_perc.protest + us_fariss_hr +
               us_war_outcome + us_growth_WDI_PW +
               us_v2x_libdem_VDEM + us.aid +
               gini_disp + fariss_hr + v2x_libdem_VDEM +
               gdppc_WDI_PW + bkcrisis_GFD,
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

# OLS, no FE 
cl.ols.chg <- lm(change_SupDem ~ 
                   Trump + Obama + W.Bush + 
                   us_perc.protest + us_fariss_hr +
                   us_war_outcome + us_growth_WDI_PW +
                   us_v2x_libdem_VDEM + us.aid +
                   gini_disp + fariss_hr + v2x_libdem_VDEM +
                   gdppc_WDI_PW + bkcrisis_GFD,
                 data = cl.data.us)
summary(cl.ols.chg)

# add country FE
cl.ols.fe <- lm(change_SupDem ~ 
                   Trump + rep_pres + us_perc.protest + us_fariss_hr +
                   us_war_outcome + us_growth_WDI_PW +
                   us_v2x_libdem_VDEM + us.aid +
                   gini_disp + fariss_hr + v2x_libdem_VDEM +
                   gdppc_WDI_PW + bkcrisis_GFD +
                  factor(ccode),
                 data = cl.data.us)
summary(cl.ols.fe)


# other presidential dummies 
cl.pres.fe <- lm(change_SupDem ~ 
                  Trump + Obama + W.Bush +
                  us_perc.protest + us_fariss_hr +
                  us_war_outcome + us_growth_WDI_PW +
                  us_v2x_libdem_VDEM + us.aid +
                  gini_disp + fariss_hr + v2x_libdem_VDEM +
                  gdppc_WDI_PW + bkcrisis_GFD +
                  factor(ccode),
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
