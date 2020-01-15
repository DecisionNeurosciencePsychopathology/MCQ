###Descriptives, plots, tables####
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
library(VIM)
library(stargazer)
library(ggplot2)
setwd("~/OneDrive/papers/discounting/data")

load('non_afsp_subs_long.Rda')
load('non_afsp_subs_wide.Rda')
load('afsp_non_pit_long.Rda')
load('afsp_non_pit_wide.Rda')
load('afsp_pit_long.Rda')
load('afsp_pit_wide.Rda')

#####Tables
setwd('~/OneDrive/papers/discounting/plots/')
####summary table 1 Pitt non AFSP
print(c1 <- c <- createTable(compareGroups(groupLeth ~ Age + Gender + Race + Ethnicity + Education + Income + highest_lethality, non_afsp_subs_wide)))
export2csv(c1, "Pitt_nonAFSP_group_characteristics.csv")
# summary table 2 Pitt AFSP
print(c2 <- createTable(compareGroups(lethgrp ~ Age + sex + RACEN + ETHNIC + educa_true + MacarthurQ6 + MAXLETH_P,
afsp_pit_wide)))
export2csv(c2, "PittAfsp_group_characteristics.csv")
# summary table 3 NYC + OH
print(c3 <- createTable(compareGroups(lethgrp ~ site_code + Age + sex + RACEN + ETHNIC + educa_true + MacarthurQ6 + MAXLETH_P,
afsp_non_pit_wide)))
export2csv(c3, "NYCandOHafsp_group_characteristics.csv")

###Descriptives
###Pitt non-AFSP
describe(non_afsp_subs_wide, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)
##Pitt AFSP 
describe(afsp_pit_wide, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)
##PNYC and OH AFSP
describe(non_pit_wide, na.rm = TRUE, interp=FALSE,skew = TRUE, ranges = TRUE,trim=.1,
         type=3,check=TRUE,fast=NULL,quant=NULL,IQR=FALSE)

#####Figure 1 psychometric curves
setwd('~/OneDrive/papers/discounting/plots/')

pdf("discounting_choice_by_k_group_pit_non_afsp.pdf", height = 6, width = 12)
ggplot(non_afsp_subs_long, aes(log(k), choice, color = groupLeth)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) 
dev.off()

pdf("discounting_choice_by_k_group_pit_afsp.pdf", height = 6, width = 12)
ggplot(afsp_pit_long, aes(log(k), choice, color = as.character(lethgrp))) + geom_smooth(method = "glm", method.args = list(family = "binomial"))
dev.off()

pdf("discounting_choice_by_k_group_non_pit_afsp_by_site.pdf", height = 6, width = 12)
ggplot(afsp_non_pit_long, aes(log(k), choice, color = as.character(lethgrp))) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(~site_code)
dev.off()

###change reference group for GLMs to high lethality
non_afsp_subs_long$lethgrp_ref_hl <- relevel(non_afsp_subs_long$groupLeth, ref = 'HL')
afsp_pit_long$lethgrp_ref_hl <- relevel(afsp_pit_long$lethgrp, ref = '3')
afsp_non_pit_long$lethgrp_ref_hl <- relevel(afsp_non_pit_long$lethgrp, ref = '3')

#####AFSP PITT
#####AFSP Pitt general model no covariates
m1 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|subject), family = binomial, afsp_pit_long, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)
# model-predicted plot AFSP Pitt general model no covariates
em <- as_tibble(emmeans::emmeans(m1, ~logk_sc | lethgrp_ref_hl, at = list(logk_sc = c(-2,2)))) %>% mutate(`Prefer later` = emmean)
setwd('~/OneDrive/papers/discounting/plots/')
pdf("modelm1.pdf", height = 6, width = 12)
ggplot(em, aes(logk_sc, `Prefer later`, color = lethgrp_ref_hl, group = lethgrp_ref_hl)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5))
dev.off()
###USE THIS ONE?
em <- as_tibble(emmeans::emtrends(m1, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelm1a.pdf", height = 6, width = 12)
ggplot(em, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5))
dev.off()
####Regression Table
stargazer(m1, type="html", out="discount_pit_afsp.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

#####AFSP NON PITT
#####AFSP Non Pitt general model no covariates
m2 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + (1|subject), family = binomial, afsp_non_pit_long,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2)
while (any(grepl("failed to converge", m2@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2,c("theta","fixef"))
  m2 <- update(m2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m2)
Anova(m2, '3')
vif(m2)
# model-predicted plot AFSP Non Pitt general model no covariates
em <- as_tibble(emmeans::emmeans(m2, ~logk_sc | lethgrp_ref_hl, at = list(logk_sc = c(-2,2)))) %>% mutate(`Prefer later` = emmean)
setwd('~/OneDrive/papers/discounting/plots/')
pdf("modelm2.pdf", height = 6, width = 12)
ggplot(em, aes(logk_sc, `Prefer later`, color = lethgrp_ref_hl, group = lethgrp_ref_hl)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5))
dev.off()
###USE THIS ONE?
em <- as_tibble(emmeans::emtrends(m2, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelm2a.pdf", height = 6, width = 12)
ggplot(em, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5))
dev.off()
####Regression Table
stargazer(m2, type="html", out="discount_non_pit_afsp.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

######
#####Pitt general model no covariates
m3 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3)
while (any(grepl("failed to converge", m3@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3,c("theta","fixef"))
  m3 <- update(m3, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m3)
Anova(m3, '3')
vif(m3)
# model-predicted plot AFSP Non Pitt general model no covariates
em <- as_tibble(emmeans::emmeans(m3, ~logk_sc | lethgrp_ref_hl, at = list(logk_sc = c(-2,2)))) %>% mutate(`Prefer later` = emmean)
setwd('~/OneDrive/papers/discounting/plots/')
pdf("modelm3.pdf", height = 6, width = 12)
ggplot(em, aes(logk_sc, `Prefer later`, color = lethgrp_ref_hl, group = lethgrp_ref_hl)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5))
dev.off()
###USE THIS ONE?
em <- as_tibble(emmeans::emtrends(m3, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelm3a.pdf", height = 6, width = 12)
ggplot(em, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + geom_line(position=position_dodge(width=0.5)) 
+ geom_point(position=position_dodge(width=0.5))
dev.off()
####Regression Table
stargazer(m1, m2, m3, type="html", out="discount_pit_non_afsp.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)
#############
########
####Sensitivity Analyses age, sex, race, income, education, substance use, and global cognitive functioning
####Pitt AFSP sample 2
#####Age
m1a <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(Age) + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1a)
while (any(grepl("failed to converge", m1a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1a,c("theta","fixef"))
  m1a <- update(m1a, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1a)
Anova(m1a, '3')
vif(m1a)
####Regression Table Pitt AFSP age
stargazer(m1a, type="html", out="discount_pit_afspAge.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Sex
m1b <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * as.factor(sex) + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1b)
while (any(grepl("failed to converge", m1b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1b,c("theta","fixef"))
  m1b <- update(m1b, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1b)
Anova(m1b, '3')
vif(m1b)
####Regression Table Pitt AFSP sex
stargazer(m1b, type="html", out="discount_pit_afspSex.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Race
m1c <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * RACEN + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1c)
while (any(grepl("failed to converge", m1c@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1c,c("theta","fixef"))
  m1c <- update(m1c, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1c)
Anova(m1c, '3')
vif(m1c)
####Regression Table Pitt AFSP race
stargazer(m1c, type="html", out="discount_pit_afspRace.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Income
m1d <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(MacarthurQ6) + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1d)
while (any(grepl("failed to converge", m1d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1d,c("theta","fixef"))
  m1d <- update(m1d, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1d)
Anova(m1d, '3')
vif(m1d)
####Regression Table Pitt AFSP Income
stargazer(m1d, type="html", out="discount_pit_afspIncome.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Education
m1e <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(educa_true) + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1e)
while (any(grepl("failed to converge", m1e@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1e,c("theta","fixef"))
  m1e <- update(m1e, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1e)
Anova(m1e, '3')
vif(m1e)
####Regression Table Pitt AFSP education
stargazer(m1e, type="html", out="discount_pit_afspEducation.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


####Substance
m1f <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * as.factor(subany) + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1f)
while (any(grepl("failed to converge", m1f@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1f,c("theta","fixef"))
  m1f <- update(m1f, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1f)
Anova(m1f, '3')
vif(m1f)
####Regression Table Pitt AFSP substance
stargazer(m1f, type="html", out="discount_pit_afspSubstance.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


####Substance
m1g <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(MMSE_tot) + (1|subject), family = binomial, afsp_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1g)
while (any(grepl("failed to converge", m1g@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1g,c("theta","fixef"))
  m1g <- update(m1g, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1g)
Anova(m1g, '3')
vif(m1g)
####Regression Table Pitt AFSP substance
stargazer(m1g, type="html", out="discount_pit_afspMMSE.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


#############
########
####Sensitivity Analyses age, sex, race, income, education, substance use, and global cognitive functioning
####Non-Pitt AFSP sample 3 (with site code!)
#####Age
m2a <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * scale(Age) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2a)
while (any(grepl("failed to converge", m2a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2a,c("theta","fixef"))
  m2a <- update(m2a, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2a)
Anova(m2a, '3')
vif(m2a)
####Regression Table Pitt AFSP age
stargazer(m2a, type="html", out="discount_non_pit_afspAge.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Sex
m2b <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * as.factor(sex) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2b)
while (any(grepl("failed to converge", m2b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2b,c("theta","fixef"))
  m2b <- update(m2b, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2b)
Anova(m2b, '3')
vif(m2b)
####Regression Table Pitt AFSP sex
stargazer(m2b, type="html", out="discount_pit_non_afspSex.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Race
m2c <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * RACEN + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2c)
while (any(grepl("failed to converge", m2c@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2c,c("theta","fixef"))
  m2c <- update(m2c, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2c)
Anova(m2c, '3')
vif(m2c)
####Regression Table Pitt AFSP race
stargazer(m2c, type="html", out="discount_non_pit_afspRace.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Income
m2d <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * scale(MacarthurQ6) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2d)
while (any(grepl("failed to converge", m2d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2d,c("theta","fixef"))
  m2d <- update(m2d, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2d)
Anova(m2d, '3')
vif(m2d)
####Regression Table Pitt AFSP Income
stargazer(m2d, type="html", out="discount_non_pit_afspIncome.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Education
m2e <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * scale(educa_true) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2e)
while (any(grepl("failed to converge", m2e@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2e,c("theta","fixef"))
  m2e <- update(m2e, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2e)
Anova(m2e, '3')
vif(m2e)
####Regression Table Pitt AFSP education
  stargazer(m2e, type="html", out="discount_non_pit_afspEducation.htm", report = "vcs*",
            digits = 2, single.row=TRUE,omit.stat = "bic",
            dep.var.labels = "Choice",
            star.char = c("*", "**", "***"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
            notes.append = F)


####Substance
m2f <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * as.factor(subany) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2f)
while (any(grepl("failed to converge", m2f@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2f,c("theta","fixef"))
  m2f <- update(m2f, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2f)
Anova(m2f, '3')
vif(m2f)
####Regression Table Pitt AFSP substance
stargazer(m2f, type="html", out="discount_non_pit_afspSubstance.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


####MMSE
m2g <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * scale(MMSE_tot) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2g)
while (any(grepl("failed to converge", m2g@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2g,c("theta","fixef"))
  m2g <- update(m2g, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m2g)
Anova(m2g, '3')
vif(m2g)
####Regression Table Pitt AFSP MMSE
stargazer(m2g, type="html", out="discount_non_pit_afspMMSE.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

#############
########
####Sensitivity Analyses age, sex, race, income, education, substance use, and global cognitive functioning
####Pitt sample 1
#####Age
m3a <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(Age) + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3a)
while (any(grepl("failed to converge", m3a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3a,c("theta","fixef"))
  m3a <- update(m3a, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3a)
Anova(m3a, '3')
vif(m3a)
####Regression Table Pitt age
stargazer(m3a, type="html", out="discount_pitAge.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Sex
m3b <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * as.factor(Gender) + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3b)
while (any(grepl("failed to converge", m3b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3b,c("theta","fixef"))
  m3b <- update(m3b, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3b)
Anova(m3b, '3')
vif(m3b)
####Regression Table Pitt sex
stargazer(m3b, type="html", out="discount_pitSex.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Race NEED TO CHANGE REFERENCE GROUP FOR RACE IT SEEMS/RECODE AS WELL?
m3c <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * Race + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3c)
while (any(grepl("failed to converge", m3c@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3c,c("theta","fixef"))
  m3c <- update(m3c, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3c)
Anova(m3c, '3')
vif(m3c)
####Regression Table Pitt race 
stargazer(m3c, type="html", out="discount_pitRace.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Income
m3d <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(Income) + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3d)
while (any(grepl("failed to converge", m3d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3d,c("theta","fixef"))
  m3d <- update(m3d, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3d)
Anova(m3d, '3')
vif(m3d)
####Regression Table Pitt Income
stargazer(m3d, type="html", out="discount_pitIncome.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####Education
m3e <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(Education) + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3e)
while (any(grepl("failed to converge", m3e@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3e,c("theta","fixef"))
  m3e <- update(m3e, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3e)
Anova(m3e, '3')
vif(m3e)
####Regression Table Pitt Education
stargazer(m3e, type="html", out="discount_pitEducation.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


####Substance NEED TO RECODE FIRST!!!
####recodes into 0 no, 1 yes substance use
non_afsp_subs_long$lifetime.subs <- recode(non_afsp_subs_long$lifetime.subs,"0=0;1=1;2=1;3=1;4=1;7=1")

m3f <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * as.factor(lifetime.subs) + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3f)
while (any(grepl("failed to converge", m3f@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3f,c("theta","fixef"))
  m3f <- update(m3f, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3f)
Anova(m3f, '3')
vif(m3f)
####Regression Table Pitt substance
stargazer(m3f, type="html", out="discount_pitSubstance.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


####MMSE
m3g <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(mmse.score) + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3g)
while (any(grepl("failed to converge", m3g@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3g,c("theta","fixef"))
  m3g <- update(m3g, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m3g)
Anova(m3g, '3')
vif(m3g)
####Regression Table Pitt MMSE
stargazer(m3g, type="html", out="discount_pitMMSE.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


#################ATTRIBUTES##########

####NOT CONVERGING for non AFSP Pitt Warning messages:
##1: In class(object) <- "environment" :
  #Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object
#2: In class(object) <- "environment" :
  #Setting class(x) to "environment" sets attribute to NULL; result will no longer be an S4 object
#3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
                  #unable to evaluate scaled gradient
               # 4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
                                  #Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

m4 <- glmer(choice ~ immMag_sc * lethgrp_ref_hl +
               delayMag_sc * lethgrp_ref_hl +
               delay_sc * lethgrp_ref_hl +
               (1|ID), family = binomial, non_afsp_subs_long)
while (any(grepl("failed to converge", m4@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4,c("theta","fixef"))
  m4 <- update(m4, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4)
Anova(m4, '3')
vif(m4)

####SAME WITH nAGQ0initStep=FALSE, still not converging

m4 <- glmer(choice ~ immMag_sc * lethgrp_ref_hl +
              delayMag_sc * lethgrp_ref_hl +
              delay_sc * lethgrp_ref_hl +
              (1|ID), family = binomial, non_afsp_subs_long)
while (any(grepl("failed to converge", m4@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4,c("theta","fixef"))
  m4 <- update(m4, start=ss, control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m4)
Anova(m4, '3')
vif(m4)



###converges for AFSP NYC+OH
m5 <- glmer(choice ~ scale(immMag) * lethgrp_ref_hl +
              scale(delayMag) * lethgrp_ref_hl +
              scale (delay) * lethgrp_ref_hl +
              (1|subject), family = binomial, afsp_non_pit_long)
while (any(grepl("failed to converge", m5@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5,c("theta","fixef"))
  m5 <- update(m5, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m5)
Anova(m5, '3')
vif(m5)

stargazer(m5, type="html", out="NonPittAFSP_Attributes.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


###converges for AFSP Pitt
m6 <- glmer(choice ~ scale(immMag) * lethgrp_ref_hl +
              scale(delayMag) * lethgrp_ref_hl +
              scale (delay) * lethgrp_ref_hl +
              (1|subject), family = binomial, afsp_non_pit_long)
while (any(grepl("failed to converge", m6@optinfo$conv$lme4$messages) )) {
  ss <- getME(m6,c("theta","fixef"))
  m6 <- update(m6, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m6)
Anova(m6, '3')
vif(m6)

stargazer(m6, type="html", out="PittAFSP_Attributes.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)
