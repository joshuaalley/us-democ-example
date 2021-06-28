# Joshua Alley and John Owen
# Load and clean data 


# Load WVS data
wvs <- readRDS("data/WVS_TimeSeries_R_v1_2.RDS")
# pull key variables w/o loading packages (example data maxes out RAM)
wvs <- select(wvs, S002, COW_NUM, COW_ALPHA,
                     S007, S020, S025, A165, C006,
                     E001, E023, E033, E069_11,
                     E111, E114, E116, E117, E119, E120, E121, E122, E123,
                     E235, E236,
                     X025A2, Y011A, Y011B, X003, X001
) 


# rename variables
wvs <- wvs %>%
             rename(
               wvs.wave = S002,
               resp.num = S007,
               ccode = COW_NUM,
               cname = COW_ALPHA,
               year = S020,
               cntry.year = S025,
               trust.gen = A165,
               financial.sat = C006,
               country.aim = E001,
               interest.pol = E023,
               left.right = E033,
               gov.conf = E069_11,
               rate.pol.sys = E111, 
               strong.ldr = E114,
               army.rule = E116,
               dem.app = E117,
               or.v.fr = E119,
               dem.econ.bad = E120,
               dem.squabble = E121,
               dem.order = E122,
               dem.prob.bet = E123,
               dem.import = E235,
               own.democ = E236,
               education = X025A2,
               resp.auth = Y011A,
               nationalism = Y011B,
               age = X003,
               gender = X001,
             ) %>%
            mutate(
              region = ifelse(ccode < 200, 1, # Americas 
                         ifelse(ccode %in% 200:400, 2, # Europe
                           ifelse(ccode > 400 & ccode < 600, 3, # subs Africa
                             ifelse(ccode >= 600 & ccode < 700, 4, # MENA
                               ifelse(ccode > 700, 5, 0))))) # Asia
            )
glimpse(wvs)

# create democratic support index like that of krieckhaus et al and others 
# 4 is very bad 
# Strong leader
table(wvs$strong.ldr)
wvs$strong.ldr <- recode(as.numeric(wvs$strong.ldr), `4` = 3, `3` = 2, `2` = 1, `1` = 0)
table(wvs$strong.ldr)
# army rule
table(wvs$army.rule)
wvs$army.rule <- recode(as.numeric(wvs$army.rule), `4` = 3, `3` = 2, `2` = 1, `1` = 0)
# approval
table(wvs$dem.app)
wvs$dem.app <- recode(as.numeric(wvs$dem.app), `4` = 0, `3` = 1, `2` = 2, `1` = 3)
table(wvs$dem.app)
# democ probably better- leave this out
table(wvs$dem.prob.bet) # only in two waves- tons of missing

# agg democ
wvs <- mutate(wvs,
              agg.democ = dem.app + army.rule + strong.ldr
              )
table(wvs$agg.democ)
# plot democratic approval
ggplot(wvs, aes(x = agg.democ)) +
  geom_bar()
summary(wvs$agg.democ)
# by wave
ggplot(wvs, aes(x = agg.democ, 
                group = factor(wvs.wave),
                fill = factor(wvs.wave))) +
  geom_bar(position = "dodge")

# high democracy dummmy
wvs$high.democ <- ifelse(wvs$agg.democ >= 7, 1, 0)
table(wvs$high.democ)

# remove the US
wvs <- filter(wvs, ccode != 2)


# look over frequency of different waves
table(wvs$wvs.wave)
summary(wvs$year)

# summarize all data: missing data already NA
summary(wvs)
# tons of missing on specific democracy attitudes 

# full missing visualization
wvs[sample(nrow(wvs), size = 10000), ] %>% 
  vis_miss()


### Impute 20 WVS survey datasets with sbgcop

# Multiple imputation of missing data with sbgcop
reg.data.wvs <- ungroup(wvs) %>%
  filter(year <= 2018 & ccode > 6) %>% # cut down to match state-level data
  select(dem.app, army.rule, strong.ldr,
         interest.pol, trust.gen, country.aim,
         left.right, gov.conf, rate.pol.sys,
         nationalism, financial.sat, resp.auth)


# set number of imputed datasets and impute 
# 100 imputed datasets: 1000/10 
impute.wvs <- sbgcop.mcmc(as.matrix(reg.data.wvs),
                           nsamp = 2000, 
                           odens = 100,
                           impute = TRUE,
                           verb = TRUE,
                           seed = 12)
summary(impute.wvs)
plot(impute.wvs)
dev.off() # reset plots

# add column names
colnames(impute.wvs$Y.impute) <- colnames(reg.data.wvs)

# Pull imputed data into a list
imputed.data.wvs <- vector(mode = "list", length = 20)
for(i in 1:length(imputed.data.wvs)){
  imputed.data.wvs[[i]] <- cbind.data.frame(select(filter(ungroup(wvs),
                                                year <= 2018 & ccode > 6), 
                                                    ccode, year),
                                             impute.wvs$Y.impute[, , i]) %>%
    mutate(
      agg.democ = dem.app + army.rule + strong.ldr,
      high.democ = ifelse(agg.democ >= 7, 1, 0)
    )
  
  # create year and observation indices
  # country ID
  imputed.data.wvs[[i]]$cntry.id <- imputed.data.wvs[[i]] %>%
    group_by(ccode) %>%
    group_indices()     
  # country-year ID
  imputed.data.wvs[[i]]$cntry.yr.id <- imputed.data.wvs[[i]] %>%
    group_by(ccode, year) %>%
    group_indices()   
  # year ID
  imputed.data.wvs[[i]]$year.id <- imputed.data.wvs[[i]] %>%
    group_by(year) %>%
    group_indices()   
}  

# look at outcome dist
ggplot(imputed.data.wvs[[1]], aes(x = high.democ)) + geom_bar()
ggplot(imputed.data.wvs[[1]], aes(x = agg.democ)) + geom_bar()



### load country and year-level data
load("data/Graham_Tucker_IPE_v4.Rdata") 

# cut down for relevant years 
ipe_v4 <- ipe_v4 %>%
           filter(year >= 1976 & year <= 2018) %>%
            select(ccode, year,
                   gdppc_WDI_PW, growth_WDI_PW, 
                   info_flow_KOF, soc_glob_KOF, bkcrisis_GFD, 
                   polity2_P4, v2x_libdem_VDEM, 
                   bdeadbest_BD)
# democracy_share_DE may be of interest later
# Check missing data
vis_miss(ipe_v4)

# NA battle deaths are no conflict obs 
ipe_v4$bdeadbest_BD[is.na(ipe_v4$bdeadbest_BD)] <- 0


# Add Fariss HR data (v3 here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/TADPGE)
fariss.hr <- read.csv("data/HumanRightsProtectionScores_v3.01.csv")

fariss.hr <- select(fariss.hr, 
                    COW, YEAR, theta_mean) %>%
              rename(
                year = YEAR,
                ccode = COW,
                fariss_hr = theta_mean
              )

ipe_v4 <- left_join(ipe_v4, fariss.hr)

# Load swiid data:
# use summary for imputation later
load("data/swiid9_0.rda")
rm(swiid) # remove list w/ latent var

# Lots of missing
# take 20 at random
swiid_summary$ccode <- countrycode(swiid_summary$country, 
                            origin = "country.name",
                            destination = "cown") 
# keep key vars
swiid_summary <- select(swiid_summary,
                        ccode, year, gini_disp)

# add GINI to IPE data
ipe_v4 <- left_join(ipe_v4, swiid_summary)



# US aid to a each country 
us.aid <- read.csv("data/us_foreign_aid_country.csv") %>%
           filter(transaction_type_name == "Disbursements" &
                  str_detect(country_name, "Region") == FALSE &
                  country_name != "China (Tibet)") %>% # aid spent
           select(country_name, fiscal_year, constant_amount) %>% # key vars only 
           rename(
             year = fiscal_year,
             us.aid = constant_amount
           )
us.aid$ccode <- countrycode(us.aid$country_name, 
                         origin = "country.name",
                         destination = "cown") 

# add us aid to IPE data
us.aid$year <- as.numeric(us.aid$year)
ipe_v4 <- left_join(ipe_v4, select(us.aid, ccode, year, us.aid))
ipe_v4$us.aid[is.na(ipe_v4$us.aid)] <- 0 # years w/ no aid missing
 

# find unique ccode-year pairs in WVS
wvs$cntry.yr.id <- wvs %>% group_by(ccode, year) %>% group_indices()
length(unique(wvs$cntry.yr.id))
state.year.data <- wvs %>%
                    filter(year <= 2018 & ccode > 6) %>% # match indiv data years
                    select(year, ccode, # pull id vars
                           cntry.yr.id) %>%
                    group_by(cntry.yr.id) %>%
                    summarize( # find ccode and year
                      ccode = unique(ccode),
                      year = unique(year),
                      .groups = "keep"
                    ) %>%
           left_join(ipe_v4) %>% # add key natl controls
            select(-(polity2_P4)) # stick to Vdem- more coverage

# duplicated: USSR/Russia from GINI
state.year.data[duplicated(state.year.data[, 1:3]), ]
state.year.data <- filter(state.year.data,
                          !(ccode == 365 & year == 1990 &
                            gini_disp == 24.4))

# visualize missing
vis_miss(state.year.data)

# calculate sd of democracy
democ.sum <- state.year.data %>%
              group_by(ccode) %>%
               summarize(
                 mean.democ = mean(v2x_libdem_VDEM, na.rm = TRUE),
                 sd.democ = sd(v2x_libdem_VDEM, na.rm = TRUE),
                 .groups = "keep"
               ) 
democ.sum$sd.democ[is.na(democ.sum$sd.democ)] <- 0 # missing w/ 1 obs
ggplot(democ.sum, aes(x = mean.democ, y = sd.democ)) + 
  geom_point()

# create consolidated democracy indicator
summary(democ.sum$mean.democ)
summary(democ.sum$sd.democ)
democ.sum <- democ.sum %>% 
               mutate(
                consol.democ = ifelse(
                 mean.democ >= .6870 & sd.democ < .1, 3, # top qauartile, low SD
                 ifelse(mean.democ >= .25 & mean.democ < .6870, 2, # middle rank
                        1) 
               )
               )
democ.sum$consol.democ[democ.sum$ccode == 232] <- 3 # Andorra
democ.sum$consol.democ[democ.sum$ccode == 260] <- 3 # FDR 
democ.sum$consol.democ[democ.sum$ccode == 714] <- 2 # Hong Kong
democ.sum$consol.democ[democ.sum$ccode == 667] <- 1 # Palestine
table(democ.sum$consol.democ)
ggplot(democ.sum, aes(x = mean.democ, y = sd.democ,
                      color = factor(consol.democ))) + 
  geom_point()



# impute state-level data
impute.state.yr <- sbgcop.mcmc(as.matrix(
                     state.year.data[, 3:ncol(state.year.data)]
                     ), # pull ccode and cntry-year IDs
                          nsamp = 2000, 
                          odens = 100,
                          impute = TRUE,
                          verb = TRUE,
                          seed = 12)
summary(impute.state.yr)
plot(impute.state.yr)
dev.off() # reset plots

# add column names
colnames(impute.state.yr$Y.impute) <- colnames(state.year.data)[3:ncol(state.year.data)]



# Pull imputed data into a list
imputed.state.yr <- vector(mode = "list", length = 20)
for(i in 1:length(imputed.state.yr)){
  imputed.state.yr[[i]] <- cbind.data.frame(select(ungroup(state.year.data), 
                                                   cntry.yr.id, ccode),
                                            impute.state.yr$Y.impute[, , i]
                                            ) 
}  

# final dataset: rescaled with constant
imputed.state.yr.final <- vector(mode = "list", length = 20)
for(i in 1:length(imputed.state.yr)){
  imputed.state.yr.final[[i]] <- select(imputed.state.yr[[i]], 
                                      -c(cntry.yr.id, year, ccode)
  )
  # rescale by 2sd
  imputed.state.yr.final[[i]] <- cbind.data.frame(
    imputed.state.yr[[i]]$ccode,
    imputed.state.yr[[i]]$year,
     apply(imputed.state.yr.final[[i]],
        2, function(x) arm::rescale(x, binary.inputs = "0/1"))
     )
  colnames(imputed.state.yr.final[[i]])[1] <- "ccode"
  colnames(imputed.state.yr.final[[i]])[2] <- "year"
}  


# summary table
state.year.sum <- bind_rows(imputed.state.yr) %>%
                  select(-c(cntry.yr.id, ccode, year))
colnames(state.year.sum) <- c("GDP per Capita", "GDP Growth", "Information Flow",
                           "Social Globalization", "Bank Crisis",
                           "Liberal Democracy", "Conflict Battle Deaths", "Human Rights",
                           "Inequality", "U.S. Aid")
datasummary_skim(state.year.sum, fmt = "%.2f",
                 title = "State Level Variables",
                 histogram = FALSE,
                 output = "appendix/state-vars.tex")


# pull us data
us.data <- filter(ipe_v4, ccode == 2)

# plot liberal democracy over time
ggplot(us.data, aes(x = year, y = v2x_libdem_VDEM)) + 
  geom_line()

# add Chinese economic growth
us.data <- cbind.data.frame(us.data,
                            select(filter(ipe_v4, ccode == 710),
                            growth_WDI_PW))
colnames(us.data)[length(colnames(us.data))] <- "chinese_growth"


# # average all over prior five years
# lag all
five.year <- apply(us.data[3:ncol(us.data)], 2, 
                  function(x) lag(x))

us.data.five <- cbind(select(us.data, year), five.year) %>%
                  filter(year >= 1980)
summary(us.data.five$year)

# create variable with indicators of war participation and victory 
us.data.five$war_outcome <- 0
us.data.five$war_outcome[us.data.five$year >= 1991 & us.data.five$year <= 1996] <- 1 # won Gulf War 1
us.data.five$war_outcome[us.data.five$year >= 2004 & us.data.five$year <= 2009] <- -1 # Iraq insurgency peak


# plot liberal democracy again
ggplot(us.data.five, aes(x = year, y = v2x_libdem_VDEM)) + 
  geom_line()

# US protests in that year (encompasses political riots)
# event root code 14 in GDELT
gdelt.protests <- read.csv("data/gdelt-protests.csv") %>% 
   filter(CountryName == "United States" & Year >= 1980)
glimpse(gdelt.protests)
# rename columns
colnames(gdelt.protests) <- c("year", "countryname", "perc.protest")


# match years and create year-level data
length(unique(state.year.data$year))
length(unique(us.data.five$year))


# presidential dummmies
us.data.five$president[us.data.five$year >= 1977 & us.data.five$year < 1981] <- "Carter"
us.data.five$president[us.data.five$year >= 1981 & us.data.five$year < 1989] <- "Reagan"
us.data.five$president[us.data.five$year >= 1989 & us.data.five$year < 1993] <- "HW Bush"
us.data.five$president[us.data.five$year >= 1993 & us.data.five$year < 2001] <- "Clinton"
us.data.five$president[us.data.five$year >= 2001 & us.data.five$year < 2009] <- "W Bush"
us.data.five$president[us.data.five$year >= 2009 & us.data.five$year < 2017] <- "Obama"
us.data.five$president[us.data.five$year >= 2017] <- "Trump"

# Presidential administration dummies
us.pres.dum <- data.frame(model.matrix(~ factor(us.data.five$president) + 0))
colnames(us.pres.dum) <- str_remove(colnames(us.pres.dum), 
                                    "factor.us.data.five.president.")
us.data.five <- cbind(us.data.five, us.pres.dum)



us.data.final <- filter(us.data.five,
                        year %in% unique(state.year.data$year)) %>%
                 mutate(
                   # add presidential partisanship
                   rep_pres = ifelse((year >= 1981 & year <= 1992) | # Reagan/Bush
                                     (year >= 2001 & year <= 2008),  # HW Bush
                                     1, 0),
                   # and cold war
                   post_cold_war = ifelse(year >= 1989 &
                                            year <= 2000, 1, 0),
                   constant = 1
                 ) %>% # add protest data
              left_join(gdelt.protests) %>% # select key
             select(constant, growth_WDI_PW, v2x_libdem_VDEM,
                    fariss_hr, perc.protest, gini_disp, 
                    Clinton, W.Bush, Obama, Trump,
                    chinese_growth, war_outcome)
vis_miss(us.data.final)

# lag fariss HR: average of last two observed years
#us.data.final$fariss_hr[
#  is.na(us.data.final$fariss_hr)] <- (0.28316380 + 0.29497110) / 2


# variation in HR and democ 
summary(us.data.final$v2x_libdem_VDEM)
sd(us.data.final$v2x_libdem_VDEM)
ggplot(us.data.final, aes(x = v2x_libdem_VDEM)) + geom_histogram()
t.test(v2x_libdem_VDEM ~ rep_pres, data = us.data.final) # higher democ ratings under dems

# latent HR score
summary(us.data.final$fariss_hr)
sd(us.data.final$fariss_hr)

# final data
glimpse(us.data.final)

# summary table
us.data.sum <- select(us.data.final, -constant)
colnames(us.data.sum) <- c("US GDP Growth", "US Democracy", "US Human Rights",
                            "US Protests", "US GINI", "Clinton", "W.Bush",
                            "Obama", "Trump Pres", "Chinese Growth",
                            "US Intervention")
datasummary_skim(us.data.sum, fmt = "%.2f",
                 title = "Year Level Variables",
                 histogram = FALSE,
                 output = "appendix/year-vars.tex")

# rescale by 2sd 
us.data.final[, 2:12] <-  apply(us.data.final[, 2:12], 2, 
                               function(x) arm::rescale(x, binary.inputs = "0/1"))
glimpse(us.data.final)

