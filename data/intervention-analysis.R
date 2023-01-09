# Joshua Alley and John Owen
# Analysis of relationship between US success
# and public support for democracy abroad 

# focus on effect of Abu Ghraib on responses in the WVS


### consider multiple potential interventions
# compare support before and after big US news

ggplot(wvs, aes(x = date)) + geom_histogram()

# Capitol riot/insurrection 
# one month
wvs.cap.1m <- filter(wvs, date >= "2020-12-06" & date <= "2021-02-06") %>%
  mutate(post.ci = ifelse(date >= "2021-01-6", 1, 0))
nrow(wvs.cap.1m)
t.test(agg.democ ~ post.ci, data = wvs.cap.1m)


# Charlottesville (August 12, 2017)
# one month
wvs.cville.1m <- filter(wvs, date >= "2017-07-12" & date <= "2017-09-12") %>%
  mutate(post.ci = ifelse(date >= "2017-08-12", 1, 0))
nrow(wvs.cville.1m)
t.test(agg.democ ~ post.ci, data = wvs.cville.1m)

# plot results
# country split- won't work
ggplot(wvs.cville.1m, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname),
              alpha = .5) +
  geom_vline(xintercept = as.numeric(as.Date("2017-08-12"))) +
  labs(x = "Date",
       y = "Democratic Support")


# obama reelection November 6, 2012
# consistent positive difference
# polls are fairly tight in national samples
# one month
wvs.obama.1m <- filter(wvs, date >= "2012-10-06" & date <= "2012-12-06") %>%
  mutate(post.ci = ifelse(date >= "2012-11-06", 1, 0))
unique(wvs.obama.1m$ccode) # decent range of countries 
t.test(agg.democ ~ post.ci, data = wvs.obama.1w)

# plot results
# Obama 2012 reelection 
# split across countries on each side- no go
ggplot(wvs.obama.1m, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname),
              alpha = .5) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2012-11-06"))) +
  labs(x = "Date",
       y = "Democratic Support")


# Snowden leak: no data
wvs.snow.1m <- filter(wvs, date >= "2012-05-13" & date <= "2012-07-13") %>%
  mutate(post.ci = ifelse(date >= "2012-06-13", 1, 0))
table(wvs.snow.1m$post.ci) # Peru
# t.test(agg.democ ~ post.ci, data = wvs.snow.1m)


# August 2019 Mass Shooting in El Paso
wvs.elpaso.1w <- filter(wvs, date >= "2019-07-27" & date <= "2019-08-10") %>%
  mutate(post.ci = ifelse(date >= "2019-08-03", 1, 0))
t.test(agg.democ ~ post.ci, data = wvs.elpaso.1w)
ggplot(wvs.elpaso.1w, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname),
              alpha = .5) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2019-08-03"))) +
  labs(x = "Date",
       y = "Democratic Support")
# one month
wvs.elpaso.1m <- filter(wvs, date >= "2019-07-03" & date <= "2019-09-03") %>%
  mutate(post.ci = ifelse(date >= "2019-08-03", 1, 0))
nrow(wvs.elpaso.1m)
t.test(agg.democ ~ post.ci, data = wvs.elpaso.1m)

ggplot(wvs.elpaso.1m, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname),
              alpha = .5) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2019-08-03"))) +
  labs(x = "Date",
       y = "Democratic Support")



# Trump admin generally
# simple dummy
wvs$trump.admin <- ifelse(wvs$date >= "2016-11-08", 1, 0)
wvs$wave.7 <- ifelse(wvs$wvs.wave == 7, 1, 0)
table(wvs$trump.admin, wvs$wave.7) # all in wave 7, starts in 2017
t.test(agg.democ ~ trump.admin, data = wvs)

# Trump Impeachment on December 18, 2019
# not consistent- maybe negative
# two days 
wvs.imp.2d <- filter(wvs, date >= "2019-12-16" & date <= "2019-12-20") %>%
  mutate(post.ci = ifelse(date >= "2019-12-18", 1, 0))
unique(wvs.imp.2d$ccode) # decent range of countries
table(wvs.imp.2d$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.2d)
# one week
wvs.imp.1w <- filter(wvs, date >= "2019-12-11" & date <= "2019-12-25") %>%
  mutate(post.ci = ifelse(date >= "2019-12-18", 1, 0))
table(wvs.imp.1w$post.ci)
unique(wvs.imp.1w$ccode) # decent range of countries 
t.test(agg.democ ~ post.ci, data = wvs.imp.1w)
# two weeks
wvs.imp.2w <- filter(wvs, date >= "2019-12-04" & date <= "2020-01-01") %>%
  mutate(post.ci = ifelse(date >= "2019-12-18", 1, 0))
table(wvs.imp.2w$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.2w)

# plot raw data
ggplot(wvs.imp.1w, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname)) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-18"))) +
  labs(x = "Date",
       y = "Democratic Support")

# focus on respondents in kyrgyzstan and nicaragua
# one week 
wvs.imp.1w.sub <- filter(wvs.imp.1w, cname == "KYR" | cname == "NIC")
table(wvs.imp.1w.sub$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.1w.sub)
summary(lm(agg.democ ~ post.ci + factor(cname), data = wvs.imp.1w.sub))
# two weeks
wvs.imp.2w.sub <- filter(wvs.imp.2w, cname == "KYR" | cname == "NIC")
table(wvs.imp.2w.sub$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.2w.sub)
summary(lm(agg.democ ~ post.ci + factor(cname), data = wvs.imp.2w.sub))

# three days
wvs.imp.3d.sub <- filter(wvs.imp.1w.sub, date >= "2019-12-15" & date <= "2019-12-21")
table(wvs.imp.3d.sub$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.3d.sub)
summary(lm(agg.democ ~ post.ci + factor(cname), data = wvs.imp.3d.sub))

# nicaragua
wvs.imp.1w.nic <- filter(wvs.imp.1w, cname == "NIC")
table(wvs.imp.1w.nic$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.1w.nic)
summary(lm(agg.democ ~ post.ci, data = wvs.imp.1w.nic))

# kyrgyzstan
wvs.imp.1w.kyr <- filter(wvs.imp.1w, cname == "KYR")
table(wvs.imp.1w.kyr$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.imp.1w.kyr)
summary(lm(agg.democ ~ post.ci, data = wvs.imp.1w.kyr))

# plot in two weeks
ggplot(wvs.imp.2w.sub, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname)) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2019-12-18"))) +
  labs(x = "Date",
       y = "Democratic Support")


# killing of Ahmaud Arbery
# one week: focus on ethiopia
wvs.arb.1w <- filter(wvs, date >= "2020-02-16" & date <= "2020-03-02" &
                       cname == "ETH") %>%
  mutate(post.ci = ifelse(date >= "2020-02-23", 1, 0))
table(wvs.arb.1w$post.ci)
vis_miss(wvs.arb.1w)
unique(wvs.arb.1w$ccode) # Ethiopia
t.test(agg.democ ~ post.ci, data = wvs.arb.1w)
# adjust for individual covar
summary(lm(agg.democ ~ post.ci + interest.pol + trust.gen + country.aim +
             gov.conf + rate.pol.sys + nationalism +
             financial.sat + resp.auth, 
           data = wvs.arb.1w))

# plot 
ggplot(wvs.arb.1w, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname)) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2020-02-23"))) +
  labs(x = "Date",
       y = "Democratic Support")

# cut down to three days
wvs.arb.3d <- filter(wvs, date >= "2020-02-20" & date <= "2020-02-26" &
                       cname == "ETH") %>%
  mutate(post.ci = ifelse(date >= "2020-02-23", 1, 0))
table(wvs.arb.3d$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.arb.3d)
# adjust for individual covar
summary(lm(agg.democ ~ post.ci + interest.pol + trust.gen + country.aim +
             gov.conf + rate.pol.sys + nationalism +
             financial.sat + resp.auth, 
           data = wvs.arb.3d))


# assassination of Quassen Solemani
# one week
# Look at Vietnam: middle of survey there 
wvs.sol.1w <- filter(wvs, date >= "2019-12-27" & date <= "2020-01-10"
                     & cname == "DRV") %>%
  mutate(post.ci = ifelse(date >= "2020-01-03", 1, 0))
table(wvs.sol.1w$post.ci)
unique(wvs.sol.1w$ccode) 
t.test(agg.democ ~ post.ci, data = wvs.sol.1w)
# adjust for individual attitudes
summary(lm(agg.democ ~ post.ci + interest.pol + trust.gen + country.aim +
           gov.conf + rate.pol.sys + nationalism +
             financial.sat + resp.auth, 
           data = wvs.sol.1w))

# plot results
ggplot(wvs.sol.1w, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname)) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-03"))) +
  labs(x = "Date",
       y = "Democratic Support")

# three days
wvs.sol.3d <- filter(wvs, date >= "2019-12-31" & date <= "2020-01-06"
                     & cname == "DRV") %>%
  mutate(post.ci = ifelse(date >= "2020-01-03", 1, 0))
table(wvs.sol.3d$post.ci)
t.test(agg.democ ~ post.ci, data = wvs.sol.3d)
# same adjustment
summary(lm(agg.democ ~ post.ci + interest.pol + trust.gen + country.aim +
             gov.conf + rate.pol.sys + nationalism +
             financial.sat + resp.auth, 
           data = wvs.sol.3d))


# bin Laden killing in 2011
# WVS in El Salvador
wvs.binl.1w <- filter(wvs, date >= "2011-04-25" & date <= "2011-05-09") %>%
  mutate(post.ci = ifelse(date >= "2011-05-02", 1, 0))
table(wvs.binl.1w$post.ci)
unique(wvs.binl.1w$ccode) # decent range of countries 
t.test(agg.democ ~ post.ci, data = wvs.binl.1w)

# plot results
ggplot(wvs.binl.1w, aes(x = date, y = agg.democ)) +
  geom_jitter(aes(color = cname)) +
  geom_smooth(method = "loess") +
  geom_vline(xintercept = as.numeric(as.Date("2011-05-02"))) +
  labs(x = "Date",
       y = "Democratic Support")


# George floyd: no data
wvs.floyd.1m <- filter(wvs, date >= "2020-04-25" & date <= "2020-06-25") %>%
  mutate(post.ci = ifelse(date >= "2020-05-25", 1, 0))

# Jacob Blake 
wvs.blake.1w <- filter(wvs, date >= "2020-08-16" & date <= "2020-08-30") %>%
  mutate(post.ci = ifelse(date >= "2020-08-23", 1, 0))


# tabulate arbery results
# one week- small difference
datasummary_balance(~ post.ci, data = select(wvs.arb.1w, post.ci,
                                             agg.democ,  interest.pol, trust.gen, country.aim,
                                             gov.conf, rate.pol.sys, nationalism, financial.sat, resp.auth),
                    fmt = 3
)
# clear in three day window
datasummary_balance(~ post.ci, data = select(wvs.arb.3d, post.ci,
                 agg.democ,  interest.pol, trust.gen, country.aim,
                 gov.conf, rate.pol.sys, nationalism, financial.sat, resp.auth),
                 fmt = 3
)

# solemani one week
datasummary_balance(~ post.ci, data = select(wvs.sol.1w, post.ci,
                  agg.democ,  interest.pol, trust.gen, country.aim,
                  gov.conf, rate.pol.sys, nationalism, financial.sat, resp.auth),
                  fmt = 3
)


### try some sort of matching
# shows nulls as well 
library(cem)


# arbery 1w window
wvs.arb.1wclean <- as.data.frame(select(wvs.arb.1w, post.ci,
                          agg.democ,  interest.pol, trust.gen, country.aim,
                          gov.conf, rate.pol.sys, nationalism, financial.sat, resp.auth) %>%
                  drop_na())
# check imbalance
imbalance(group = wvs.arb.1wclean$post.ci, 
          data = wvs.arb.1wclean)
# match arbery data
match.arb <- cem(treatment = "post.ci", data = wvs.arb.1wclean, 
                drop = "agg.democ",
                keep.all = TRUE)
match.arb
relax.cem(match.arb, wvs.arb.1wclean, depth = 2, perc = 0.2)


# re-run the algorithm w/ user-specified bins for aim and financial sat
match.arb <- cem(treatment = "post.ci", data = wvs.arb.1wclean, 
                cutpoints = list(rate.pol.sys = 2, financial.sat = 4), drop = "agg.democ",
                keep.all = TRUE)
match.arb

# lm 
lm.att.arb <- att(match.arb, agg.democ ~ post.ci + interest.pol + trust.gen + country.aim +
                    gov.conf + rate.pol.sys + nationalism +
                    financial.sat + resp.auth, 
                 data = wvs.arb.1wclean, model = "linear")
summary(lm.att.arb)
plot(lm.att.arb, match.arb, wvs.arb.1wclean)

# random forest
rf.att.arb <- att(match.arb, agg.democ ~ post.ci + interest.pol + trust.gen + country.aim +
                    gov.conf + rate.pol.sys + nationalism +
                    financial.sat + resp.auth, 
                 data = wvs.arb.1wclean, model = "forest")
summary(rf.att.arb)
plot(rf.att.arb, match.arb, wvs.arb.1wclean)
