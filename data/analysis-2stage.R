# Joshua Alley and John Owen
# Two-stage analysis 


# US data without 2sd rescale
# helps with differences
us.data.long.2s <- filter(us.data.five,
                        year %in% unique(state.year.data$year)) %>%
  mutate(
    # add presidential partisanship
    rep_pres = ifelse((year >= 1981 & year <= 1992) | # Reagan/Bush
                        (year >= 2001 & year <= 2008),  # HW Bush
                      1, 0),
    trump = ifelse((year >= 2017), 1, 0), # Trump
    # and cold war
    post_cold_war = ifelse(year >= 1991, 1, 0),
    constant = 1
  ) %>% # add protest data
  left_join(gdelt.protests) %>% # select key
  select(growth_WDI_PW, v2x_libdem_VDEM,
         fariss_hr, perc.protest, gini_disp, rep_pres, trump,
         post_cold_war, chinese_growth, war_outcome)

us.data.long.2s <-  left_join(select(imputed.state.yr[[1]], ccode, year), 
          cbind.data.frame(us.data.long.2s, filter(us.data.five,
                                                 year %in% unique(state.year.data$year))%>%
                             select(year)))%>%
  select(-c(ccode, year))
# modify US data variable labels 
colnames(us.data.long.2s) <- c("us.growth", "us.democ", "us.hr",
                               "us.pro", "us.gini", "rep.pres", "trump",
                               "post.cw", "china.growth",
                               "us.war")


# stage one- individual regressions by country
glimpse(imputed.data.wvs[[1]])
names(imputed.data.wvs) <- c(1:20)


# fit lms by country-year,
# generate fitted values, and combine with other data
# using function within lapply 
indiv.fit <- lapply(seq_along(imputed.data.wvs), 
                    function(y, n, j){
 data = y[[j]]
 fit <- vector(mode = "list", 
                      length = length(unique(data$cntry.yr.id)))
 for(i in 1:length(unique(data$cntry.yr.id))){
  # set id
  id <- unique(data$cntry.yr.id)[i]
  # fit model              
  fit[[i]] <- selm(agg.democ ~ interest.pol + country.aim + left.right +
                         gov.conf + rate.pol.sys + nationalism + financial.sat + 
                         resp.auth,
                       family = "SN",
                       data = filter(data, cntry.yr.id == id)
  )
 }
 # fitted values with variables at median or mean
 pred.data <- as.matrix(c(1, 3, 1, 5, 3, 5, .18, 6, .26))   
 indiv.pred <- as.vector(
   unlist(
   lapply(fit, function(x) mean(fitted(x)))#coef(x) %*% pred.data)
   ),
   mode = "numeric")
 # combine data 
 draw = as.numeric(n[[j]]) # draw index   
  fitted.data <- cbind.data.frame(indiv.pred,
                                  imputed.state.yr[[draw]][, 3:11],
                                  us.data.long.2s)  %>%
    mutate(
      diff.growth = us.growth - growth_WDI_PW,
      diff.democ = us.democ - v2x_libdem_VDEM,
      diff.hr = us.hr - fariss_hr
    )
}, # end function
y = imputed.data.wvs,
n = names(imputed.data.wvs)
)

# fit the state-year model
fit.list <- lapply(indiv.fit, function(x)
   lm(indiv.pred ~ us.growth + us.democ + us.hr +
       us.pro + us.gini + rep.pres + trump +
       post.cw + china.growth + us.war +
       gdppc_WDI_PW + growth_WDI_PW + info_flow_KOF +
       soc_glob_KOF + v2x_libdem_VDEM +
       bdeadbest_BD + fariss_hr, 
     data = x))

# pool results using Rubin's rule from mice package
imputed.res.full <- as.mira(fit.list)
pool.mi.fit <- pool(imputed.res.full)
pool.mi.fit
summary(pool.mi.fit)


# fit the model
fit.list.diff <- lapply(indiv.fit, function(x)
  lm(indiv.pred ~ diff.growth + diff.democ + diff.hr +
                    us.pro + us.gini + rep.pres + trump +
                    post.cw + china.growth + us.war +
                    gdppc_WDI_PW + info_flow_KOF +
                    soc_glob_KOF + bdeadbest_BD, 
     data = x))

# pool results using Rubin's rule from mice package
imputed.res.diff <- as.mira(fit.list.diff)
pool.mi.diff <- pool(imputed.res.diff)
pool.mi.diff
summary(pool.mi.diff)



