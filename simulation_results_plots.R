# run SimulationMCQ_Kirby.R first
source('~/code/MCQ/SimulationMCQ_Kirby.R')

MCQ_choices_long1_renamed$noise <- noises[1]
MCQ_choices_long2_renamed$noise <- noises[2]
MCQ_choices_long2_renamed$ID <- MCQ_choices_long2_renamed$ID + 200
MCQ_choices_long3_renamed$noise <- noises[3]
MCQ_choices_long3_renamed$ID <- MCQ_choices_long3_renamed$ID + 400
MCQ_choices_long4_renamed$noise <- noises[4]
MCQ_choices_long4_renamed$ID <- MCQ_choices_long4_renamed$ID + 600

ldf <- rbind(MCQ_choices_long1_renamed,
             MCQ_choices_long2_renamed,
             MCQ_choices_long3_renamed,
             MCQ_choices_long4_renamed)
# ldf <- MCQ_choices_long_combined
ldf <- ldf %>% mutate(log_k_true = case_when(
  group==1 ~'-3.95',
  group==2 ~ '-5.64'
), noise = as.factor(noise))


# df <- SubjectLevelCombinedWithRE
sub_df1$noise <- noises[1]
sub_df2$noise <- noises[2]
sub_df3$noise <- noises[3]
sub_df4$noise <- noises[4]
df <- rbind(sub_df1, sub_df2, sub_df3, sub_df4)
setwd('~/code/MCQ/')

# s <- load('Workspace_combined.RData')
df <- df %>% mutate(log_k_true = case_when(
  group==1 ~'-3.95',
  group==2 ~ '-5.64'
), noise = as.factor(noise))



# glmer on simulated data

m1 <- glmer(choice ~ logk_sc * noise + logk_sc * log_k_true  +
              (1|ID), family = binomial, ldf, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)

em1 <- as_tibble(emmeans(m1, specs  = c("logk_sc", "noise", "log_k_true")))
em1$choice_tendency <- em1$emmean

ggplot(em1, aes(log_k_true, choice_tendency, color = noise)) + geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL))

# boxplot of recovered by original discount rate
# 
# em2 <- as_tibble(emmeans(m1, specs  = c("logk_sc", "noise", "log_k_true")))
# em2$choice_tendency <- em2$emmean
# ggplot(em2, aes(log_k_true, choice_tendency, color = noise)) + geom_point() + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL))
# 
# p1 <- ggplot(df, aes(as.factor(log_k_true), log_k_sub)) + 
#   geom_boxplot() + facet_wrap(~noise)
# p2 <- ggplot(df, aes(as.factor(log_k_true), -InterceptRE)) + 
#   geom_boxplot() + facet_wrap(~noise)
# ggarrange(p1,p2)
# 
# p3 <- ggplot(df, aes(as.factor(noise), max_consistency)) + 
#   geom_boxplot()
# p4 <- ggplot(df, aes(as.factor(noise), LogkRE)) + 
#   geom_boxplot()
# ggarrange(p3,p4)


# the log_k - consistency correlation is also present for Kirby
m2 <- lm(log_k_sub ~ log_k_true * noise, df)
summary(m2)
Anova(m2, type = '3')

m3 <- lm(max_consistency ~ log_k_true * noise, df)
summary(m3)
Anova(m3, type = '3')

######Z SCORES
###z scores glmer
library(tidyverse)

A<- c(32.328, -1.948, -14.489, -18.640)
B<- c("log_k_true", "noise=0.1", "noise=0.33", "noise=0.67")
d <- as_tibble(A, names = "statistic")
d$method <- 'MLM'
d$predictor <- B
# barplot(A, names.arg=B, ylab="z score")
###converting t to z for lm predicting log_k_sub (z=(t-50)/10)
C<-c(-6.8789, -4.9308, -4.8407, -5.0534)
# the vector of Kirby/lm statistic should have (1) effect of true k on recovered k and (2-4) effects of true noise levels on consistencies
D<-c("log_k_true", "noise=0.1", "noise=0.33", "noise=0.67")
# barplot(C, names.arg=D, ylab="z score")
c <- as_tibble(C, names = "statistic")
c$method = 'Kirby'
c$predictor <- D
d <- rbind(d,c)
ggplot(d, aes(predictor, statistic, color = method)) + geom_bar()
####converting t to z scores lm predicting max_consistency (z=(t-50)/10)
O<-c(-5, -5.0591,-6.1044, -6.9861)
P<-c("log_k_true", "noise=0.1", "noise=0.33", "noise=0.67")
barplot(O, names.arg=P, ylab="t score")




