

df <- SubjectLevelCombinedWithRE
ldf <- MCQ_choices_long_combined


df <- df %>% mutate(log_k_true = case_when(
  group_k==1 ~'-3.95',
  group_k==2 ~ '-5.64'
))

# boxplot of recovered by original discount rate

p1 <- ggplot(df, aes(as.factor(log_k_true), log_k_sub)) + 
  geom_boxplot() + facet_wrap(~noise)
p2 <- ggplot(df, aes(as.factor(log_k_true), -InterceptRE)) + 
  geom_boxplot() + facet_wrap(~noise)
ggarrange(p1,p2)

p1 <- ggplot(df, aes(as.factor(noise), max_consistency)) + 
  geom_boxplot()
p2 <- ggplot(df, aes(as.factor(noise), LogkRE)) + 
  geom_boxplot()
ggarrange(p1,p2)
