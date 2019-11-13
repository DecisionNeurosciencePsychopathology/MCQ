####### Read in tall-format delay discounting data, build multi-level models
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
setwd("~/OneDrive/papers/discounting/data")
load('discounting_processed_afsp.Rdata')
# sanity check on consistency
# ggplot(df %>% filter(!is.na(groupLeth)), aes(log(k), consistency, color = groupLeth)) + geom_smooth(method = "gam", formula = y ~ splines::ns(x,4))

# check missingness
library(mice)
df_aggr = aggr(adf, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# df_aggr = aggr(sub_df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(sub_df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# summary(lm(log_k_sub ~ groupLeth, orig_subs))
# talk to Jiazhou about changing permissions for installing compareGroups
print(c1 <- createTable(compareGroups(lethgrp ~ site_code + Age + sex + RACEN + ETHNIC + educa_true + MacarthurQ6 + MAXLETH_P + IDEATION, sub_df %>% filter(site_code!=2))))
export2html(c1, "afsp_group_characteristics.html")

print(c2 <- createTable(compareGroups(site_code ~ Age + sex + RACEN + ETHNIC + educa_true + MacarthurQ6 + MAXLETH_P + IDEATION, sub_df)))


setwd('~/OneDrive/papers/discounting/plots/')
pdf("discounting_choice_by_k_group_afsp_by_site.pdf", height = 6, width = 12)
# remove group 'NA' for now
ggplot(adf %>% filter(!is.na(lethgrp)), aes(log(k), choice, color = as.character(lethgrp))) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(~site_code)
dev.off()
ggplot(df, aes(log(k), choice, color = groupLeth)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(~immMag >50)

pdf("discounting_choice_by_afsp_by_site.pdf", height = 6, width = 8)
# remove group 'NA' for now
ggplot(adf %>% filter(!is.na(lethgrp)), aes(log(k), choice, color = as.character(site_code))) + geom_smooth(method = "glm", method.args = list(family = "binomial")) 
dev.off()


# initial model looking at the three sites -- now converging!
# 1 = NYC, 2 = PGH, 3 = Columbus
m1 <- glmer(choice ~ logk_sc * lethgrp + logk_sc * site_code + (1|subject), family = binomial, adf, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)

# without Pittsburgh
m2 <- glmer(choice ~ logk_sc * lethgrp + logk_sc * site_code + (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2@optinfo$conv$lme4$messages) )) {
  print(m2@optinfo$conv$lme4$conv)
  ss <- getME(m2,c("theta","fixef"))
  m2 <- update(m2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m2)
Anova(m2, '3')

# and with demo covariates
m2a <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2a@optinfo$conv$lme4$messages) )) {
  print(m2a@optinfo$conv$lme4$messages)
  ss <- getME(m2a,c("theta","fixef"))
  m2a <- update(m2a, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))}
summary(m2a)
Anova(m2a, '3')

m2b <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + logk_sc * scale(MacarthurQ6) + (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2b@optinfo$conv$lme4$messages) )) {
  print(m2b@optinfo$conv$lme4$messages)
  ss <- getME(m2b,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m2b <- update(m2b, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m2b)
Anova(m2b, '3')
# income is NS, drop

# race
m2c <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + logk_sc * RACEN + (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2c@optinfo$conv$lme4$messages) )) {
  print(m2c@optinfo$conv$lme4$messages)
  ss <- getME(m2c,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m2c <- update(m2c, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m2c)
Anova(m2c, '3')
# race NS, drop

# age has a positive effect on sensitivity
m2d <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + logk_sc * scale(Age) + (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2d@optinfo$conv$lme4$messages) )) {
  print(m2d@optinfo$conv$lme4$messages)
  ss <- getME(m2d,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m2d <- update(m2d, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = c("nloptwrap", "bobyqa"),optCtr=list(maxfun=2e6)))} 
summary(m2d)
Anova(m2d, '3')
vif(m2d)

# sex is NS, drop
m2e <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + logk_sc * scale(Age) +  logk_sc * as.factor(sex) + (1|subject), family = binomial, adf %>% filter(site_code!=2),
             control = glmerControl(optimizer = "nloptwrap",nAGQ0initStep=FALSE))
while (any(grepl("failed to converge", m2e@optinfo$conv$lme4$messages) )) {
  print(m2e@optinfo$conv$lme4$messages)
  ss <- getME(m2e,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m2e <- update(m2e, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = c("nloptwrap", "bobyqa"),optCtr=list(maxfun=2e6)))} 
summary(m2e)
Anova(m2e, '3')

# MMSE interestingly NS
m2f <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + logk_sc * scale(Age) +  logk_sc * scale(MMSE_tot) + (1|subject), family = binomial, adf %>% filter(site_code!=2),
             control = glmerControl(optimizer = "nloptwrap",nAGQ0initStep=FALSE))
while (any(grepl("failed to converge", m2f@optinfo$conv$lme4$messages) )) {
  print(m2f@optinfo$conv$lme4$messages)
  ss <- getME(m2f,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m2f <- update(m2f, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = c("nloptwrap", "bobyqa"),optCtr=list(maxfun=2e6)))} 
summary(m2f)
Anova(m2f, '3')

# substance -- NS, drop
m2g <- glmer(choice ~ logk_sc * lethgrp +logk_sc * site_code +  logk_sc * scale(educa_true) + logk_sc * scale(Age) +  logk_sc * as.factor(subany) + (1|subject), family = binomial, adf %>% filter(site_code!=2),
             control = glmerControl(optimizer = "nloptwrap",nAGQ0initStep=FALSE))
while (any(grepl("failed to converge", m2g@optinfo$conv$lme4$messages) )) {
  print(m2g@optinfo$conv$lme4$messages)
  ss <- getME(m2g,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m2g <- update(m2g, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = c("nloptwrap", "bobyqa"),optCtr=list(maxfun=2e6)))} 
summary(m2g)
Anova(m2g, '3')


# attributes -- converges if you avoid the delay * education interaction
m3a <- glmer(choice ~ immMag_sc * lethgrp + immMag_sc * scale(educa_true) + delayMag_sc * lethgrp + delayMag_sc * scale(educa_true) +
               delay_sc * lethgrp + 
               (1|subject), family = binomial, adf %>% filter(site_code!=2),
             control = glmerControl(optimizer = "nloptwrap",nAGQ0initStep=FALSE))
while (any(grepl("failed to converge", m3a@optinfo$conv$lme4$messages) )) {
  print(m3a@optinfo$conv$lme4$messages)
  ss <- getME(m3a,c("theta","fixef"))
  # magic ingredient: nAGQ0initStep=FALSE to get good initial estimates!
  m3a <- update(m3a, start=ss,  control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = c("nloptwrap", "bobyqa"),optCtr=list(maxfun=2e6)))} 
summary(m3a)
Anova(m3a, '3')
vif(m3a)

############
# additional Pittsburgh models not run on AFSP



# go back to log(k), check cognitive confounds

m5a <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender  + logk_sc*exit_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5a,c("theta","fixef"))
  m5a <- update(m5a, start=ss, control=glmerControl(optCtr=list(maxfun=1e5)))}
summary(m5a)
Anova(m5a, '3')
vif(m5a)

m5b <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc*drs_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5b,c("theta","fixef"))
  m5b <- update(m5b, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=1e5)))}
summary(m5b)
Anova(m5b, '3')
vif(m5b)

m5c <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc*wtar_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5c@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5c,c("theta","fixef"))
  m5c <- update(m5c, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=1e5)))}
summary(m5c)
Anova(m5c, '3')
vif(m5c)

m5d <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc*mmse_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5d,c("theta","fixef"))
  m5d <- update(m5d, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=1e5)))}
summary(m5d)
Anova(m5d, '3')
vif(m5d)

setwd('~/OneDrive/papers/discounting/data/')
save(file = "discounting_results_afsp.Rdata",list = ls(all.names = TRUE))
# load("discounting_results.Rdata")
