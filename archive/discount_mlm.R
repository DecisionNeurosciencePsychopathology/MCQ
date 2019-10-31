####### Read in tall-format delay discounting data, build multi-level models
library(dplyr)
library(tidyverse)
library(psych)
library(corrplot)
library(lme4)
library(ggpubr)
library(car)
library(readxl)
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
  groupLeth = as.factor(groupLeth))
df$choice[df$choice==2] <- NA
df$Income[df$Income>10] <- NA

# TO DO:
# calculate subject-wise ks


sub_df <- df %>% select(ID, Group, groupLeth, Age, Gender, Race, Ethnicity, Education, Marital.status, 
                        Income, ham17.score, SSI.score, SIS.score, drs.score, wtar.score, exit.score, highest_lethality) %>% unique()
# talk to Jiazhou about changing permissions for installing compareGroups

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


m1 <- glmer(choice ~ delayMag_sc + immMag_sc + delay_sc + (1 + delay_sc|ID), family = binomial, df)
summary(m1)
Anova(m1, '3')
vif(m1)
# alternative models
# m2 <- glmer(choice ~ delayMag_sc + magRatio_sc + delay_sc + (1 + delay_sc|ID), family = binomial, df)
# summary(m2)
# vif(m2)
# m3 <- glmer(choice ~ (delayMag_sc + magRatio_sc + delay_sc)^3 + (1|ID), family = binomial, df)
# summary(m3)
# vif(m3)
# 
m4 <- glmer(choice ~ logk_sc + (1|ID), family = binomial, df)
summary(m4)
Anova(m4, '3')
vif(m4)
# anova(m1,m4)
m1a <- glmer(choice ~ delayMag_sc + immMag_sc + delay_sc + Group + Gender + Income + Education + (1 + delay_sc|ID), family = binomial, df)
# m1a <- glmer(choice ~ delayMag_sc*Group + immMag_sc*Group + delay_sc*Group +
#                delayMag_sc*Gender + immMag_sc*Gender + delay_sc*Gender + 
#                delayMag_sc*Income + immMag_sc*Income + delay_sc*Income + 
#                delayMag_sc*Education + immMag_sc*Education + delay_sc*Education + (1 + delay_sc|ID), family = binomial, df)
summary(m1a)
Anova(m1a, '3')
vif(m1a)

m4a <- glmer(choice ~ logk_sc + Group + Gender + Income + Education + (1|ID), family = binomial, df)
summary(m4a)
Anova(m4a, '3')
vif(m4a)
m4b <- glmer(choice ~ logk_sc + groupLeth + Gender + Income + Education + (1|ID), family = binomial, df)
summary(m4b)
Anova(m4b, '3')
vif(m4b)
m4c <- glmer(choice ~ logk_sc * groupLeth + Gender + Income + Education + (1|ID), family = binomial, df)
summary(m4c)
Anova(m4c, '3')
ss <- getME(m4c,c("theta","fixef"))
m4c <- update(m4c, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(m4c)
Anova(m4c, '3')

m4d <- glmer(choice ~ logk_sc * groupLeth + logk_sc *Gender + logk_sc *Income + logk_sc *Education + (1|ID), family = binomial, df)
summary(m4d)
Anova(m4d, '3')
ss <- getME(m4d,c("theta","fixef"))
m4d <- update(m4d, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))
summary(m4d)
Anova(m4d, '3')


m1b <- glmer(choice ~ delayMag_sc + immMag_sc + delay_sc + groupLeth + Gender + Income + Education + (1 + delay_sc|ID), family = binomial, df)

# m1b <- glmer(choice ~ delayMag_sc*groupLeth + immMag_sc*groupLeth + delay_sc*groupLeth +
#                delayMag_sc*Gender + immMag_sc*Gender + delay_sc*Gender + 
#                delayMag_sc*Income + immMag_sc*Income + delay_sc*Income + 
#                delayMag_sc*Education + immMag_sc*Education + delay_sc*Education + (1 + delay_sc|ID), family = binomial, df)
summary(m1b)
Anova(m1b, '3')

