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

###Models controlling for Age, sex, education, race, income

###Recode race variable into binary
####not working currently for some reason/returns error that object "X6" not found
#non_afsp_subs_long$Race <- recode(non_afsp_subs_long$Race,"X2=0; X3=0; X5=1; X6=0")

afsp_pit_long<-afsp_pit_long %>% mutate(RaceBin = case_when(
  RACEN == '2' ~ '0',
  RACEN == '4' ~ '0',
  RACEN == '5' ~ '1',
  RACEN== '6' ~ '0'
))

afsp_non_pit_long<-afsp_non_pit_long %>% mutate(RaceBin = case_when(
  RACEN == '1' ~ '0',
  RACEN == '2' ~ '0',
  RACEN == '3' ~ '0',
  RACEN == '4' ~ '0',
  RACEN == '5' ~ '1',
  RACEN == '6' ~ '0',
  RACEN == '7' ~ '0',
))


###afsp_pit_long$RACEN <- recode(afsp_pit_long$RACEN,"2=0; 4=0; 5=1; 6=0")
###afsp_non_pit_long$RACEN <- recode(afsp_non_pit_long$RACEN,"1=0; 2=0; 3=0; 4=0; 5=1; 6=0; 7=0")

########Error in recode(non_afsp_subs_long$Race, "X2=0; X3=0; X6=0; X5=1") : 
#in recode term:  X5=1
#message: Error in eval(parse(text = strsplit(term, "=")[[1]][1])) : 
  #object 'X5' not found

non_afsp_subs_long <-  non_afsp_subs_long %>% mutate(RaceBin = case_when(
  Race == 'X2' ~ '0',
  Race == 'X3' ~ '0',
  Race == 'X6' ~ '0',
  Race == 'X5' ~ '1'
))


table(non_afsp_subs_long$RaceBin)
table(afsp_pit_long$RaceBin)
table(afsp_non_pit_long$RaceBin)

###change reference group for GLMs to high lethality
non_afsp_subs_long$lethgrp_ref_hl <- relevel(non_afsp_subs_long$groupLeth, ref = 'HL')
afsp_pit_long$lethgrp_ref_hl <- relevel(afsp_pit_long$lethgrp, ref = '3')
afsp_non_pit_long$lethgrp_ref_hl <- relevel(afsp_non_pit_long$lethgrp, ref = '3')

setwd('~/OneDrive/papers/discounting/plots/')

###Pitt Non AFSP, Race binary---now converges!
m7 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * scale(Age) + logk_sc * as.factor(RaceBin) + logk_sc * scale(Education) + 
              logk_sc * scale(Income) + logk_sc * as.factor(Gender) + (1|ID), family = binomial, non_afsp_subs_long, 
            control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m7)
while (any(grepl("failed to converge", m7@optinfo$conv$lme4$messages) )) {
  ss <- getME(m7,c("theta","fixef"))
  m7 <- update(m7, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m7)
Anova(m7, '3')
vif(m7)

####Regression Table
stargazer(m7, type="html", out="discount_pit_non_afsp_covs.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

###Pitt AFSP 
###Model failed to converge with max|grad| = 0.0169537 (tol = 0.001, component 1)
m7a <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * as.factor(RACEN) + logk_sc * scale(Age) + 
               logk_sc * as.factor(sex) + logk_sc * scale(educa_true) + logk_sc * scale(MacarthurQ6) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m7a)
while (any(grepl("failed to converge", m7a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m7a,c("theta","fixef"))
  m7b <- update(m7a, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m7a)
Anova(m7a, '3')
vif(m7a)

####Regression Table
stargazer(m7a, type="html", out="discount_pit_afsp_covs.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

###Non Pitt AFSP with site code---converges
m7b <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + logk_sc * as.factor(RACEN) + logk_sc * scale(Age) + 
               logk_sc * as.factor(sex) + logk_sc * scale(educa_true) + logk_sc * scale(MacarthurQ6) + (1|subject), family = binomial, afsp_non_pit_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m7b)
while (any(grepl("failed to converge", m7b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m7b,c("theta","fixef"))
  m7b <- update(m7b, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m7b)
Anova(m7b, '3')
vif(m7b)

####Regression Table
stargazer(m7b, type="html", out="discount_non_pit_afsp_covs.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)



#######################
######################Rerunning main models without those who only chose all 0s or all 1s
afsp_pit_long_filtered<-filter(afsp_pit_long, !subject %in% c("212856", "220104", "219675", "220597", "220772", "221295"))
afsp_non_pit_long_filtered<-filter(afsp_non_pit_long, !subject %in% c("15", "59", "72", "103", "22042", "22154", "83", "105", "22244"))
non_afsp_subs_long_filtered<-filter(non_afsp_subs_long, !ID %in% c("203803","208474","208485","208735","209581","209635","209660","209951", "210873", "212586",
                                                                   "212856", "216603", "217988", "219675", "220104", "220513", "220597", "220678", "220772", 
                                                                   "221037", "221167", "221181", "221292", "221295", "221423", "221681", "221726", "222198", "431023", "431209"))

###change reference group for GLMs to high lethality
afsp_pit_long_filtered$lethgrp_ref_hl <- relevel(afsp_pit_long_filtered$lethgrp, ref = '3')
afsp_non_pit_long_filtered$lethgrp_ref_hl <- relevel(afsp_non_pit_long_filtered$lethgrp, ref = '3')
non_afsp_subs_long_filtered$lethgrp_ref_hl <- relevel(non_afsp_subs_long_filtered$groupLeth, ref = 'HL')

#####AFSP Pitt, general model, no covariates, without those who only chose 0s or 1s
m8 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|subject), family = binomial, afsp_pit_long_filtered, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m8@optinfo$conv$lme4$messages) )) {
  ss <- getME(m8,c("theta","fixef"))
  m8 <- update(m8, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m8)
Anova(m8, '3')
vif(m8)

####Regression Table
stargazer(m8, type="html", out="discount_pit_afsp_withoutAlLZerosOrAllOnes.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

#####AFSP NON PITT
#####AFSP Non Pitt general model no covariates, without those who only chose all 0s or all 1s
m8a <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + (1|subject), family = binomial, afsp_non_pit_long_filtered,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m8a)
while (any(grepl("failed to converge", m8a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m8a,c("theta","fixef"))
  m8a <- update(m8a, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m8a)
Anova(m8a, '3')
vif(m8a)

####Regression Table
stargazer(m8a, type="html", out="discount_non_pit_afsp_withoutAlLZerosOrAllOnes.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)


######
#####Pitt general model no covariates without those who only chose all zeros or all ones
m8b <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long_filtered,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m8b)
while (any(grepl("failed to converge", m8b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m8b,c("theta","fixef"))
  m8b <- update(m8b, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m8b)
Anova(m8b, '3')
vif(m8b)

####Regression Table
stargazer(m8b, type="html", out="discount_pit_non_afsp_withoutAlLZerosOrAllOnes.htm", report = "vcs*",
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

######without delay_sc

m4 <- glmer(choice ~ immMag_sc * lethgrp_ref_hl +
              delayMag_sc * lethgrp_ref_hl +
              (1|ID), family = binomial, non_afsp_subs_long)
while (any(grepl("failed to converge", m4@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4,c("theta","fixef"))
  m4 <- update(m4, start=ss, control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m4)
Anova(m4, '3')
vif(m4)

stargazer(m4, type="html", out="NonAFSP_AttributesWithoutDelay.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

####One attribute interaction at a time Pitt NON AFSP

m4a <- glmer(choice ~ immMag_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long)
while (any(grepl("failed to converge", m4a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4a,c("theta","fixef"))
  m4a <- update(m4a, start=ss, control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m4a)
Anova(m4a, '3')
vif(m4a)

stargazer(m4a, type="html", out="NonPittAFSP_AttributesImmMagOnly.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

m4b <- glmer(choice ~ delayMag_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long)
while (any(grepl("failed to converge", m4b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4b,c("theta","fixef"))
  m4b <- update(m4b, start=ss, control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m4b)
Anova(m4b, '3')
vif(m4b)

stargazer(m4b, type="html", out="NonPittAFSP_AttributesDelayMagOnly.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

m4c <- glmer(choice ~ delay_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long)
while (any(grepl("failed to converge", m4c@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4c,c("theta","fixef"))
  m4 <- update(m4c, start=ss, control=glmerControl(nAGQ0initStep=FALSE,restart_edge = FALSE, optimizer = "nloptwrap",optCtr=list(maxfun=2e6)))} 
summary(m4c)
Anova(m4c, '3')
vif(m4c)

stargazer(m4c, type="html", out="NonPittAFSP_AttributesDelayOnly.htm", report = "vcs*",
          digits = 2, single.row=TRUE,omit.stat = "bic",
          dep.var.labels = "Choice",
          star.char = c("*", "**", "***"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("* p<0.05; ** p<0.01; *** p<0.001"),
          notes.append = F)

###converges for AFSP NYC+OH---check on site code
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


###AFSP Pitt
m6 <- glmer(choice ~ scale(immMag) * lethgrp_ref_hl +
              scale(delayMag) * lethgrp_ref_hl +
              scale (delay) * lethgrp_ref_hl +
              (1|subject), family = binomial, afsp_pit_long)
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
###USE THIS ONE
em <- as_tibble(emmeans::emtrends(m1, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelm1a.pdf", height = 3, width = 6)
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
pdf("modelm2a.pdf", height = 3, width = 6)
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
pdf("modelm3a.pdf", height = 3, width = 6)
ggplot(em, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + geom_line(position=position_dodge(width=0.5)) 
+ geom_point(position=position_dodge(width=0.5))
dev.off()
####Regression Table
stargazer(m3, type="html", out="discount_pit_non_afsp.htm", report = "vcs*",
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


####MMSE
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

#####CURVE FIGURES

# Wes Anderson version
library(wesanderson)

##Pitt Non AFSP
non_afsp_subs_long <- rename(non_afsp_subs_long, Lethality = "groupLeth")
non_afsp_subs_long<-non_afsp_subs_long %>% mutate(Lethality = case_when(
  Lethality == 'HC' ~ 'Controls',
  Lethality == 'DEP' ~ 'MDD',
  Lethality == 'IDE' ~ 'MDD+SI',
  Lethality == 'LL' ~ 'Low Lethality SA',
  Lethality == 'HL' ~ "High Lethality SA"
))
non_afsp_subs_long$Lethality <- factor(non_afsp_subs_long$Lethality, levels = c("Controls", "MDD", "MDD+SI", "Low Lethality SA", "High Lethality SA"))

pal = wes_palette("Zissou1", type = "discrete")
pdf("discounting_choice_by_k_group_pit_non_afsp_wa.pdf", width = 4, height = 3.5)
ggplot(non_afsp_subs_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()

##Pitt AFSP

afsp_pit_long <- rename(afsp_pit_long, Lethality = "lethgrp")
afsp_pit_long<-afsp_pit_long %>% mutate(Lethality = case_when(
  Lethality == '0' ~ 'Controls',
  Lethality == '1' ~ 'MDD',
  Lethality == '2' ~ 'Low Lethality SA',
  Lethality == '3' ~ "High Lethality SA"
))
afsp_pit_long$Lethality <- factor(afsp_pit_long$Lethality, levels = c("Controls", "MDD", "Low Lethality SA", "High Lethality SA"))

pal = wes_palette("Zissou1", 4, type = "continuous")
pdf("discounting_choice_by_k_group_pit_afsp_wa.pdf", width = 4, height = 3.5)
ggplot(afsp_pit_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()






##Non-Pitt AFSP
afsp_non_pit_long <- rename(afsp_non_pit_long, Lethality = "lethgrp")
afsp_non_pit_long<-afsp_non_pit_long %>% mutate(Lethality = case_when(
  Lethality == '0' ~ 'Controls',
  Lethality == '1' ~ 'MDD',
  Lethality == '2' ~ 'Low Lethality SA',
  Lethality == '3' ~ "High Lethality SA"
))
afsp_non_pit_long$Lethality <- factor(afsp_non_pit_long$Lethality, levels = c("Controls", "MDD", "Low Lethality SA", "High Lethality SA"))

pal = wes_palette("Zissou1", 4, type = "continuous")
pdf("discounting_choice_by_k_group_non_pit_afsp_wa.pdf", width = 4, height = 3.5)
ggplot(afsp_non_pit_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()



# variance in random intercept (time preference) vs. random slope of logK (value sensitivity)

rm1 <- glmer(choice ~ logk_sc + (1 + logk_sc|ID), family = binomial, non_afsp_subs_long, 
            control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(rm1)

m100 <- glmer(choice ~ logk_sc + (1|ID), family = binomial, non_afsp_subs_long, 
             control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
m101 <- glmer(choice ~ logk_sc + lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long, 
              control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
m102 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long, 
              control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
anova(m100, m101, m102)


