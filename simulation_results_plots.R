

df <- SubjectLevelCombinedWithRE
ldf <- MCQ_choices_long_combined


df <- df %>% mutate(log_k_true = case_when(
  group_k==1 ~'-3.95',
  group_k==2 ~ '-5.64'
))

ldf <- ldf %>% mutate(log_k_true = case_when(
  group_k==1 ~'-3.95',
  group_k==2 ~ '-5.64'
))

# boxplot of recovered by original discount rate

p1 <- ggplot(df, aes(as.factor(log_k_true), log_k_sub)) + 
  geom_boxplot() + facet_wrap(~noise)
p2 <- ggplot(df, aes(as.factor(log_k_true), -InterceptRE)) + 
  geom_boxplot() + facet_wrap(~noise)
ggarrange(p1,p2)

p3 <- ggplot(df, aes(as.factor(noise), max_consistency)) + 
  geom_boxplot()
p4 <- ggplot(df, aes(as.factor(noise), LogkRE)) + 
  geom_boxplot()
ggarrange(p3,p4)

m1 <- glmer(choice ~ logk_sc * noise + logk_sc * log_k_true  + 
              (1|ID), family = binomial, ldf, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)
car::Anova(m1, '3')


m2 <- lm(log_k_sub ~ log_k_true * noise, df)
summary(m2)
Anova(m2)

m3 <- lm(max_consistency ~ log_k_true * noise, df)
summary(m3)
Anova(m3)

