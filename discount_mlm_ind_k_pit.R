####### Read in tall-format delay discounting data, build multi-level models
# runs on Pittsburgh data
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
load('discounting_processed_pit.Rdata')
# sanity check on consistency
ggplot(df %>% filter(!is.na(groupLeth)), aes(log(k), consistency, color = groupLeth)) + geom_smooth(method = "gam", formula = y ~ splines::ns(x,4))

# check missingness
library(VIM)
df_aggr = aggr(df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))



sub_df <- df %>% select(ID, Group, groupLeth, k_sub, log_k_sub, max_consistency, Age, Gender, Race, Ethnicity, Education, Marital.status, 
                        Income, ham17.score, SSI.score, SIS.score, drs.score, wtar.score, exit.score, mmse.score, highest_lethality, dom2011overlap, afspoverlap) %>% unique()
orig_subs <- sub_df %>% filter(dom2011overlap==1) # subjects in Dombrovski et al. 2011
new_subs <- sub_df %>% filter(dom2011overlap==0) # Pittsburgh older (50+) subjects not in Dombrovski 2011
non_afsp_subs <- sub_df %>% filter(afspoverlap==0)
df_aggr = aggr(sub_df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(sub_df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# summary(lm(log_k_sub ~ groupLeth, orig_subs))
# talk to Jiazhou about changing permissions for installing compareGroups
print(c1 <- createTable(compareGroups(groupLeth ~ log_k_sub + max_consistency + Age + Gender + Race + Education + Income + SIS.score + SSI.score, sub_df)))
print(c2 <- createTable(compareGroups(groupLeth ~ log_k_sub + max_consistency + Age + Gender + Race + Education + Income + SIS.score + SSI.score, orig_subs)))
print(c3 <- createTable(compareGroups(groupLeth ~ log_k_sub + max_consistency + Age + Gender + Race + Education + Income + SIS.score + SSI.score, new_subs)))
print(c4 <- c <- createTable(compareGroups(groupLeth ~ log_k_sub + max_consistency + Age + Gender + Race + Education + Income + SIS.score + SSI.score, non_afsp_subs)))
export2html(c4, "non_afsp_group_characteristics.html")
# reproduce the Dombrovski et al. Biol Psych 2011 boxplot
ggplot(orig_subs, aes(groupLeth, log_k_sub)) + geom_boxplot()

cormat <- corr.test(df %>% select(delayMag_sc, immMag_sc, delay_sc, magRatio_sc, k, logk))
corrplot(cormat$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "hclust", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cormat$p, sig.level=0.05, insig = "blank")

ggplot(df, aes(log(k), choice, color = delayMag>50)) + geom_smooth(method = "glm", method.args = list(family = "binomial"))
ggplot(df, aes(delay, choice, color = immMag>50)) + geom_smooth(method = "glm", method.args = list(family = "binomial"))
ggplot(df, aes(log(magRatio), choice)) + geom_smooth(method = "glm", method.args = list(family = "binomial"))
ggplot(df, aes(log(k), choice)) + geom_smooth(method = "glm", method.args = list(family = "binomial"))

setwd('~/OneDrive/papers/discounting/plots/')
pdf("discounting_choice_by_k_group.pdf", height = 6, width = 8)
ggplot(df %>% filter(!is.na(groupLeth)), aes(log(k), choice, color = groupLeth)) + geom_smooth(method = "glm", method.args = list(family = "binomial"))
dev.off()
ggplot(df, aes(log(k), choice, color = groupLeth)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(~immMag >50)

m4a <- glmer(choice ~ logk_sc * groupLeth + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m4a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4a,c("theta","fixef"))
  m4a <- update(m4a, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4a)
Anova(m4a, '3')
vif(m4a)

# without AFSP subjects
m4a1 <- glmer(choice ~ logk_sc * groupLeth + (1|ID), family = binomial, df %>% filter(afspoverlap==0))
while (any(grepl("failed to converge", m4a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4a1,c("theta","fixef"))
  m4a1 <- update(m4a1, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4a1)
Anova(m4a1, '3')

# without AFSP subjects
m4b1 <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *education_sc + (1|ID), family = binomial, df %>% filter(afspoverlap==0))
while (any(grepl("failed to converge", m4b1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4b1,c("theta","fixef"))
  m4b <- update(m4b1, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4b1)
Anova(m4b1, '3')


m4d <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *income_sc + logk_sc *education_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m4d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4d,c("theta","fixef"))
  m4d <- update(m4d, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4d)
Anova(m4d, '3')
vif(m4d)

# replication sample only
m4dr <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *income_sc + logk_sc *education_sc + (1|ID), family = binomial, df %>% filter(overlap==0))
while (any(grepl("failed to converge", m4d@optinfo$conv$lme4$messages) )) {
  ss <- getME(m4dr,c("theta","fixef"))
  m4dr <- update(m4dr, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m4dr)
Anova(m4dr, '3')
vif(m4dr)


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

# item number -- these models don't converge
# m6a <- glmer(choice ~ logk_sc * groupLeth + scale(item) + (1|ID), family = binomial, df)
# while (any(grepl("failed to converge", m6a@optinfo$conv$lme4$messages) )) {
#   ss <- getME(m6a,c("theta","fixef"))
#   m6a <- update(m6a, start=ss, control=glmerControl(optCtr=list(maxfun=1e5)))}
# summary(m6a)
# Anova(m6a, '3')
# vif(m6a)
setwd('~/OneDrive/papers/discounting/data/')
save(file = "discounting_results.Rdata",list = ls(all.names = TRUE))
# load("discounting_results.Rdata")
