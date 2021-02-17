
library(dplyr)
library(tidyverse)
library(psych)
library(corrplot)
environment(nloptwrap)$defaultControl
library(lme4)
library(ggpubr)
library(car)
library(readxl)
library(compareGroups)
library(optimx)
library(VIM)
library(stargazer)
library(ggplot2)
library(wesanderson)# rstan ----
library(rstan)
library(rstanarm)
library(bayestestR)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
setwd("~/OneDrive/discounting/data")

load('non_afsp_subs_long.Rda')
load('afsp_non_pit_long.Rda')
load('afsp_pit_long.Rda')

###change reference group for GLMs to high lethality

non_afsp_subs_long$lethgrp_ref_hl <- relevel(non_afsp_subs_long$groupLeth, ref = 'HL')
afsp_pit_long$lethgrp_ref_hl <- relevel(afsp_pit_long$lethgrp, ref = '3')
afsp_non_pit_long$lethgrp_ref_hl <- relevel(afsp_non_pit_long$lethgrp, ref = '3')

#################ATTRIBUTES##########
##NON-AFSP sample 1

m0 <- stan_glmer (choice ~ (1|ID), family = binomial, data = non_afsp_subs_long, chains = 4, iter = 4000,  diagnostic_file=file.path(tempdir(), "df.csv"))
m1<- stan_glmer (choice ~ immMag_sc * lethgrp_ref_hl + (1|ID), family = binomial, data = non_afsp_subs_long, chains = 4, iter = 4000, diagnostic_file=file.path(tempdir(), "df.csv"))
m2<- stan_glmer (choice ~ delayMag_sc * lethgrp_ref_hl + (1|ID), family = binomial, data = non_afsp_subs_long, chains = 4, iter = 4000, diagnostic_file=file.path(tempdir(), "df.csv"))
m3<- stan_glmer (choice ~ delay_sc * lethgrp_ref_hl + (1|ID), family = binomial, data = non_afsp_subs_long, chains = 4, iter = 4000, diagnostic_file=file.path(tempdir(), "df.csv"))


summary(m0)
post0a<-describe_posterior(m0, centrality = 'median', test = c('p_direction', 'p_significance'))
post0b <-insight::get_parameters(m0)
describe_posterior(post0b)
prior_summary(m0)

summary(m1)
post1a<-describe_posterior(m1, centrality = 'median', test = c('p_direction', 'p_significance'))
post1b <-insight::get_parameters(m1)
describe_posterior(post1b)
prior_summary(m1)

summary(m2)
post2a<-describe_posterior(m2, centrality = 'median', test = c('p_direction', 'p_significance'))
post2b <-insight::get_parameters(m2)
describe_posterior(post2b)
prior_summary(m2)

summary(m3)
post3a<-describe_posterior(m3, centrality = 'median', test = c('p_direction', 'p_significance'))
post3b <-insight::get_parameters(m3)
describe_posterior(post3b)
prior_summary(m3)

comparison<-bayesfactor_models(m1, m2, m3, denominator = m0)
comparison
as.matrix(comparison)

comparison1<-bayesfactor_models(m0, m1, m2, denominator = m3)
comparison1
as.matrix(comparison1)


