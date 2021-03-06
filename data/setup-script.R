# Joshua Alley and John Owen
# set up data-cleaning and analysis of US example


# load packages
library(conflicted)
library(tidyverse)
library(lubridate)
library(sbgcop)
library(naniar)
library(countrycode)
library(cmdstanr)
library(bayesplot)
library(loo)
color_scheme_set("darkgray")
library(shinystan)
library(posterior)
library(gridExtra)
library(modelsummary)
library(brms)
library(wesanderson)
 
# manage conflicts 
conflict_scout()
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("Position", "ggplot2")
conflict_prefer("extract", "rstan")
conflict_prefer("rhat", "posterior")
conflict_prefer("ess_bulk", "posterior")
conflict_prefer("ess_tail", "posterior")
conflict_prefer("combine", "dplyr")

# set seed
set.seed(12)

# set default ggplot theme
theme_set(theme_bw())

