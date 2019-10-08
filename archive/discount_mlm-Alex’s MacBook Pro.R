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
# Pittsburgh data
df <- as_tibble(read.csv("~/OneDrive/papers/discounting/data/MCQwithdemo.csv"))  %>% 
  dplyr::rename(immMag = Immediate.magnitude, 
                delayMag = Delayed.magnitude,
                delay = Lenth.of.delay,
                choice = Response,
                item = Item) %>% mutate(magRatio = delayMag/immMag,
                                          logMagRatio = log(magRatio),
                                          k = (magRatio-1)/delay,
                                          logk = log(k))

# AFSP data
# df <- read_excel("~/OneDrive/papers/discounting/afsp_multisite_MCQ.xlsx") %>% dplyr::rename(immMag = `Immediate magnitude`, 
#                                                                                       delayMag = `Delayed magnitude`,
#                                                                                       delay = `Length of delay`,
#                                                                                       choice = `Response`,
#                                                                                       item = `Item`) %>% mutate(magRatio = delayMag/immMag,
#                                                                                                                 logMagRatio = log(magRatio),
#                                                                                                                 k = (magRatio-1)/delay,
#                                                                                                                 logk = log(k))
df$Income[df$Income>10] <- NA
df$Education[df$Education>25] <- NA
df$wtar.score[df$wtar.score>200] <- NA
df <- df %>% mutate(choiceChar = case_when(
  choice==0 ~ 'now',
  choice==1 ~ 'later',
  choice==2 ~ 'NA'),
  delayMag_sc = scale(delayMag),
  delay_sc = scale(delay),
  immMag_sc = scale(immMag),
  magRatio_sc = scale(magRatio),
  logMagRatio_sc = scale(logMagRatio),
  logk_sc = scale(logk),
  groupLeth = case_when(
    Group=='ATT' & highest_lethality>3 ~ 'HL',
    Group=='ATT' & highest_lethality<4 ~ 'LL',
    Group=='DEP' ~ 'DEP',
    Group=='IDE' ~ 'IDE',
    Group=='HC' ~ 'HC'),
  groupLeth = as.factor(groupLeth),
  age_sc = scale(Age),
  education_sc = scale(Education),
  drs_sc = scale(drs.score),
  wtar_sc = scale(wtar.score),
  exit_sc = scale(exit.score),
  income_sc = scale(Income))
df$choice[df$choice==2] <- NA

# TO DO:
# calculate subject-wise ks


sub_df <- df %>% select(ID, Group, groupLeth, Age, Gender, Race, Ethnicity, Education, Marital.status, 
                        Income, ham17.score, SSI.score, SIS.score, drs.score, wtar.score, exit.score, highest_lethality) %>% unique()
# talk to Jiazhou about changing permissions for installing compareGroups
createTable(compareGroups(groupLeth ~ Age + Gender + Race + Education + Income + SIS.score + SSI.score, sub_df))
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


# m1 <- glmer(choice ~ delayMag_sc + immMag_sc + delay_sc + (1 + delay_sc|ID), family = binomial, df)
# summary(m1)
# Anova(m1, '3')
# vif(m1)
# alternative models
# m2 <- glmer(choice ~ delayMag_sc + magRatio_sc + delay_sc + (1 + delay_sc|ID), family = binomial, df)
# summary(m2)
# vif(m2)
# m3 <- glmer(choice ~ (delayMag_sc + magRatio_sc + delay_sc)^3 + (1|ID), family = binomial, df)
# summary(m3)
# vif(m3)
# 
# m4 <- glmer(choice ~ logk_sc + (1|ID), family = binomial, df)
# summary(m4)
# Anova(m4, '3')
# vif(m4)
# anova(m1,m4)
# m1a <- glmer(choice ~ delayMag_sc + immMag_sc + delay_sc + Group + Gender + Income + Education + (1 + delay_sc|ID), family = binomial, df)
# # m1a <- glmer(choice ~ delayMag_sc*Group + immMag_sc*Group + delay_sc*Group +
# #                delayMag_sc*Gender + immMag_sc*Gender + delay_sc*Gender + 
# #                delayMag_sc*Income + immMag_sc*Income + delay_sc*Income + 
# #                delayMag_sc*Education + immMag_sc*Education + delay_sc*Education + (1 + delay_sc|ID), family = binomial, df)
# summary(m1a)
# Anova(m1a, '3')
# vif(m1a)

# m4a <- glmer(choice ~ logk_sc + Group + Gender + Income + Education + (1|ID), family = binomial, df)
# summary(m4a)
# Anova(m4a, '3')
# vif(m4a)
# m4b <- glmer(choice ~ logk_sc + groupLeth + Gender + Income + Education + (1|ID), family = binomial, df)
# summary(m4b)
# Anova(m4b, '3')
# vif(m4b)
# m4c <- glmer(choice ~ logk_sc * groupLeth + Gender + Income + Education + (1|ID), family = binomial, df)
# summary(m4c)
# Anova(m4c, '3')
# ss <- getME(m4c,c("theta","fixef"))
# m4c <- update(m4c, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
# summary(m4c)
# Anova(m4c, '3')

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

m5a <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *income_sc + logk_sc *education_sc + logk_sc*exit_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5a@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5a,c("theta","fixef"))
  m5a <- update(m5a, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m5a)
Anova(m5a, '3')
vif(m5a)

m5b <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *income_sc + logk_sc *education_sc + logk_sc*drs_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5b@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5b,c("theta","fixef"))
  m5b <- update(m5b, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m5b)
Anova(m5b, '3')
vif(m5b)

m5c <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *income_sc + logk_sc *education_sc + logk_sc*wtar_sc + (1|ID), family = binomial, df)
while (any(grepl("failed to converge", m5c@optinfo$conv$lme4$messages) )) {
  ss <- getME(m5c,c("theta","fixef"))
  m5c <- update(m5c, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m5c)
Anova(m5c, '3')
vif(m5c)

save(file = "discounting_results.Rdata")
