####### Read in tall-format delay discounting data, build multi-level models
library(dplyr)
library(tidyverse)
library(psych)
library(corrplot)
library(lme4)
library(ggpubr)
library(car)
library(readxl)
library(compareGroups)
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


# initial model looking at the three sites
# 1 = NYC, 2 = PGH, 3 = Columbus
m1 <- glmer(choice ~ logk_sc * lethgrp + logk_sc * site_code + (1|site_code/subject), family = binomial, adf)
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)

# without Pittsburgh
m2 <- glmer(choice ~ logk_sc * lethgrp + logk_sc * site_code + (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2,c("theta","fixef"))
  m2 <- update(m2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m2)
Anova(m2, '3')

# and with demo covariates
m2a <- glmer(choice ~ logk_sc * lethgrp + logk_sc * site_code + 
               logk_sc * RACEN + logk_sc * educa_true + logk_sc * as.numeric(MacarthurQ6) + 
               (1|subject), family = binomial, adf %>% filter(site_code!=2))
while (any(grepl("failed to converge", m2a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2a,c("theta","fixef"))
  m2a <- update(m2a, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m2a)
Anova(m2a, '3')


## models below run in the Pittsburgh sample previously and not adapted to the AFSP data

m4d <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *income_sc + logk_sc *education_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m4d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4d,c("theta","fixef"))
  m4d <- update(m4d, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4d)
Anova(m4d, '3')
vif(m4d)


# run models on other attributes
# immediate
m4e <- glmer(choice ~ immMag_sc * groupLeth + immMag_sc *Gender + immMag_sc *income_sc + immMag_sc *education_sc + (1|ID), family = binomial, df)
summary(m4e)
Anova(m4e, '3')
ss <- getME(m4e,c("theta","fixef"))
m4e <- update(m4e, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(m4e)
Anova(m4e, '3')
vif(m4e)

# delayed mag
m4f <- glmer(choice ~ delayMag_sc * groupLeth + delayMag_sc *Gender + delayMag_sc *income_sc + delayMag_sc *education_sc + (1|ID), family = binomial, df)
summary(m4f)
Anova(m4f, '3')
ss <- getME(m4f,c("theta","fixef"))
m4f <- update(m4f, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(m4f)
Anova(m4f, '3')
vif(m4f)

# delay
m4g <- glmer(choice ~ delay_sc * groupLeth + delay_sc *Gender + delay_sc *income_sc + delay_sc *education_sc + (1|ID), family = binomial, df)
summary(m4g)
Anova(m4g, '3')
ss <- getME(m4g,c("theta","fixef"))
m4g <- update(m4g, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(m4g)
Anova(m4g, '3')
vif(m4g)

#all
m4h <- glmer(choice ~ immMag_sc * groupLeth + immMag_sc *Gender + immMag_sc *income_sc + immMag_sc *education_sc + 
               delayMag_sc * groupLeth + delayMag_sc *Gender + delayMag_sc *income_sc + delayMag_sc *education_sc +
               delay_sc * groupLeth + delay_sc *Gender + delay_sc *income_sc + delay_sc *education_sc + 
               (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m4h@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4h,c("theta","fixef"))
  m4h <- update(m4h, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4h)
Anova(m4h, '3')
vif(m4h)


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
