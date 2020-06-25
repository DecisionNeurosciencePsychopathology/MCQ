library(bbmle)
library(lme4)
library(tidyr)
library(corrplot)
library(dplyr)
library(tidyverse)
library(psych)
library(corrplot)
library(lme4)
library(ggpubr)
library(car)
library(readxl)
library(compareGroups)
library(haven)

num_subjects=100 #number of subjects per group
# set up choices from MCQ
MCQ_options=matrix(data=c(
  54,55,117,
  55,75,61,
  19,25,53,
  84.5,85,155,
  14,25,19,
  47,50,160,
  15,35,13,
  25,60,14,
  78,80,162,
  40,55,62,
  34.75,35,186,
  67,75,119,
  34,35,186,
  27,50,21,
  69,85,91,
  49,60,89,
  80,85,157,
  24,35,29,
  33,80,14,
  28,30,179,
  34,50,30,
  25,30,80,
  41,75,20,
  54,60,111,
  54,80,30,
  22,25,136,
  59.75,60,109,
  34.5,35,186,
  84,85,150,
  59.5,60,108),nrow=30,byrow=TRUE) 
MCQ_options=as.data.frame(MCQ_options)
names(MCQ_options)=c('imm_reward','delay_reward','delay')

#compute discount rate for indifference point
MCQ_options$k_values=(MCQ_options$delay_reward/MCQ_options$imm_reward-1)/MCQ_options$delay
MCQ_options$logk=log(MCQ_options$k_values)
MCQ_options$logk_sc=scale(MCQ_options$logk)

###assigning values--means of controls and LL from 2011 paper
discounted_values_grp1=MCQ_options$delay_reward*
  (1/(1+exp(-3.95)*MCQ_options$delay))

discounted_values_grp2=MCQ_options$delay_reward*
  (1/(1+exp(-5.64)*MCQ_options$delay))

noises <- c(0, .33, .67, .1)
####Groups 1 and 2, noise 0
MCQ_choices1=array(data=NA,dim=c(num_subjects*2,30))
for (s in 1:num_subjects) {
  for (q in 1:30) {
    #group 1 randomly picks a value, mean is imm reward and SD is based on noise times that reward
    noisy_imm_reward1_1=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value1_1=rnorm(1,discounted_values_grp1[q],noises[1]*discounted_values_grp1[q]) 
    
    MCQ_choices1[s,q]=ifelse(noisy_imm_reward1_1>noisy_discounted_value1_1,1,0) #1 for imm, 0 for delayed  (if first argument is true, it assigns the value of the second; if immreward is higher, then choice 1; if discounte d value is higher it's 0)
    
    #group 2 
    noisy_imm_reward2_1=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value2_1=rnorm(1,discounted_values_grp2[q],noises[1]*discounted_values_grp2[q])
    
    MCQ_choices1[(num_subjects+s),q]=ifelse(noisy_imm_reward2_1>noisy_discounted_value2_1,1,0) 
  }
}
MCQ_choices1=data.frame(MCQ_choices1)
names(MCQ_choices1)=paste0('Q',seq(1,30,by=1))
MCQ_choices_long1=pivot_longer(MCQ_choices1,cols=names(MCQ_choices1),names_to='Question')
MCQ_choices_long1$Question=as.numeric(sub('^Q','',MCQ_choices_long1$Question))
MCQ_choices_long1$k=MCQ_options$k_values[MCQ_choices_long1$Question]
MCQ_choices_long1$logk_sc=MCQ_options$logk_sc[MCQ_choices_long1$Question]
MCQ_choices_long1$group=as.factor(c(rep(1,num_subjects*30),rep(2,num_subjects*30)))
MCQ_choices_long1$ID=c(rep(1:(2*num_subjects),each=30))

######Kirby's for noise level 0
MCQ_choices_long1_renamed<-MCQ_choices_long1 %>% 
  rename(choice = value)
df<-MCQ_choices_long1_renamed

df <- df %>% arrange(ID, k)
ks <- unique(df$k)
ids <- unique(df$ID)
df$consistency <- NA
df$k_sub <- NA
df$max_consistency <- NA
for (id in ids) {
  for (k in ks) {
    df$consistency[df$ID==id & df$k==k] = (sum(df$ID==id & df$k>k & df$choice==0, na.rm = T) + sum(df$ID==id & df$k<k & df$choice==1, na.rm = T))/(sum(!is.na(df$choice[df$ID==id]))-1)
  }
  best <- df %>% filter(ID==id & consistency == max(consistency[ID==id])) %>% select(k, consistency)
  df$k_sub[df$ID==id] <- geometric.mean(best$k)
  df$max_consistency[df$ID==id] <- max(best$consistency)
}
df$log_k_sub = log(df$k_sub)

sub_df1 <- df %>% select(ID, k_sub, log_k_sub, max_consistency, group) %>% unique()



####Groups 1 and 2, noise 0.33
MCQ_choices2=array(data=NA,dim=c(num_subjects*2,30))
for (s in 1:num_subjects) {
  for (q in 1:30) {
    #group 1 randomly picks a value, mean is imm reward and SD is based on noise times that reward
    noisy_imm_reward1_2=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value1_2=rnorm(1,discounted_values_grp1[q],noises[2]*discounted_values_grp1[q]) 
    
    MCQ_choices2[s,q]=ifelse(noisy_imm_reward1_2>noisy_discounted_value1_2,1,0) #1 for imm, 0 for delayed  (if first argument is true, it assigns the value of the second; if immreward is higher, then choice 1; if discounte d value is higher it's 0)
    
    #group 2 
    noisy_imm_reward2_2=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value2_2=rnorm(1,discounted_values_grp2[q],noises[2]*discounted_values_grp2[q])
    
    MCQ_choices2[(num_subjects+s),q]=ifelse(noisy_imm_reward2_2>noisy_discounted_value2_2,1,0) 
  }
}
MCQ_choices2=data.frame(MCQ_choices2)
names(MCQ_choices2)=paste0('Q',seq(1,30,by=1))
MCQ_choices_long2=pivot_longer(MCQ_choices2,cols=names(MCQ_choices2),names_to='Question')
MCQ_choices_long2$Question=as.numeric(sub('^Q','',MCQ_choices_long2$Question))
MCQ_choices_long2$k=MCQ_options$k_values[MCQ_choices_long2$Question]
MCQ_choices_long2$logk_sc=MCQ_options$logk_sc[MCQ_choices_long2$Question]
MCQ_choices_long2$group=as.factor(c(rep(1,num_subjects*30),rep(2,num_subjects*30)))
MCQ_choices_long2$ID=c(rep(1:(2*num_subjects),each=30))

######Kirby's for noise level noises[2]
MCQ_choices_long2_renamed<-MCQ_choices_long2 %>% 
  rename(choice = value)
df<-MCQ_choices_long2_renamed

df <- df %>% arrange(ID, k)
ks <- unique(df$k)
ids <- unique(df$ID)
df$consistency <- NA
df$k_sub <- NA
df$max_consistency <- NA
for (id in ids) {
  for (k in ks) {
    df$consistency[df$ID==id & df$k==k] = (sum(df$ID==id & df$k>k & df$choice==0, na.rm = T) + sum(df$ID==id & df$k<k & df$choice==1, na.rm = T))/(sum(!is.na(df$choice[df$ID==id]))-1)
  }
  best <- df %>% filter(ID==id & consistency == max(consistency[ID==id])) %>% select(k, consistency)
  df$k_sub[df$ID==id] <- geometric.mean(best$k)
  df$max_consistency[df$ID==id] <- max(best$consistency)
}
df$log_k_sub = log(df$k_sub)
sub_df2 <- df %>% select(ID, k_sub, log_k_sub, max_consistency, group) %>% unique()

####Groups 1 and 2, noise noises[3]
MCQ_choices3=array(data=NA,dim=c(num_subjects*2,30))
for (s in 1:num_subjects) {
  for (q in 1:30) {
    #group 1 randomly picks a value, mean is imm reward and SD is based on noise times that reward
    noisy_imm_reward1_3=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value1_3=rnorm(1,discounted_values_grp1[q],noises[3]*discounted_values_grp1[q]) 
    
    MCQ_choices3[s,q]=ifelse(noisy_imm_reward1_3>noisy_discounted_value1_3,1,0) #1 for imm, 0 for delayed  (if first argument is true, it assigns the value of the second; if immreward is higher, then choice 1; if discounte d value is higher it's 0)
    
    #group 2 
    noisy_imm_reward2_3=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value2_3=rnorm(1,discounted_values_grp2[q],noises[3]*discounted_values_grp2[q])
    
    MCQ_choices3[(num_subjects+s),q]=ifelse(noisy_imm_reward2_3>noisy_discounted_value2_3,1,0) 
  }
}
MCQ_choices3=data.frame(MCQ_choices3)
names(MCQ_choices3)=paste0('Q',seq(1,30,by=1))
MCQ_choices_long3=pivot_longer(MCQ_choices3,cols=names(MCQ_choices3),names_to='Question')
MCQ_choices_long3$Question=as.numeric(sub('^Q','',MCQ_choices_long3$Question))
MCQ_choices_long3$k=MCQ_options$k_values[MCQ_choices_long3$Question]
MCQ_choices_long3$logk_sc=MCQ_options$logk_sc[MCQ_choices_long3$Question]
MCQ_choices_long3$group=as.factor(c(rep(1,num_subjects*30),rep(2,num_subjects*30)))
MCQ_choices_long3$ID=c(rep(1:(2*num_subjects),each=30))

######Kirby's for noise level 0.33
MCQ_choices_long3_renamed<-MCQ_choices_long3 %>% 
  rename(choice = value)
df<-MCQ_choices_long3_renamed
df <- df %>% arrange(ID, k)
ks <- unique(df$k)
ids <- unique(df$ID)
df$consistency <- NA
df$k_sub <- NA
df$max_consistency <- NA
for (id in ids) {
  for (k in ks) {
    df$consistency[df$ID==id & df$k==k] = (sum(df$ID==id & df$k>k & df$choice==0, na.rm = T) + sum(df$ID==id & df$k<k & df$choice==1, na.rm = T))/(sum(!is.na(df$choice[df$ID==id]))-1)
  }
  best <- df %>% filter(ID==id & consistency == max(consistency[ID==id])) %>% select(k, consistency)
  df$k_sub[df$ID==id] <- geometric.mean(best$k)
  df$max_consistency[df$ID==id] <- max(best$consistency)
}
df$log_k_sub = log(df$k_sub)
sub_df3 <- df %>% select(ID, k_sub, log_k_sub, max_consistency, group) %>% unique()


####Groups 1 and 2, noise 1
MCQ_choices4=array(data=NA,dim=c(num_subjects*2,30))
for (s in 1:num_subjects) {
  for (q in 1:30) {
    #group 1 randomly picks a value, mean is imm reward and SD is based on noise times that reward
    noisy_imm_reward1_4=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value1_4=rnorm(1,discounted_values_grp1[q],noises[4]*discounted_values_grp1[q]) 
    
    MCQ_choices4[s,q]=ifelse(noisy_imm_reward1_4>noisy_discounted_value1_4,1,0) #1 for imm, 0 for delayed  (if first argument is true, it assigns the value of the second; if immreward is higher, then choice 1; if discounte d value is higher it's 0)
    
    #group 2 
    noisy_imm_reward2_4=rnorm(1,MCQ_options$imm_reward[q],0*MCQ_options$imm_reward[q])
    noisy_discounted_value2_4=rnorm(1,discounted_values_grp2[q],noises[4]*discounted_values_grp2[q])
    
    MCQ_choices4[(num_subjects+s),q]=ifelse(noisy_imm_reward2_4>noisy_discounted_value2_4,1,0) 
  }
}
MCQ_choices4=data.frame(MCQ_choices4)
names(MCQ_choices4)=paste0('Q',seq(1,30,by=1))
MCQ_choices_long4=pivot_longer(MCQ_choices4,cols=names(MCQ_choices4),names_to='Question')
MCQ_choices_long4$Question=as.numeric(sub('^Q','',MCQ_choices_long4$Question))
MCQ_choices_long4$k=MCQ_options$k_values[MCQ_choices_long4$Question]
MCQ_choices_long4$logk_sc=MCQ_options$logk_sc[MCQ_choices_long4$Question]
MCQ_choices_long4$group=as.factor(c(rep(1,num_subjects*30),rep(2,num_subjects*30)))
MCQ_choices_long4$ID=c(rep(1:(2*num_subjects),each=30))

######Kirby's for noise level 0.33
MCQ_choices_long4_renamed<-MCQ_choices_long4 %>% 
  rename(choice = value)
df<-MCQ_choices_long4_renamed
df <- df %>% arrange(ID, k)
ks <- unique(df$k)
ids <- unique(df$ID)
df$consistency <- NA
df$k_sub <- NA
df$max_consistency <- NA
for (id in ids) {
  for (k in ks) {
    df$consistency[df$ID==id & df$k==k] = (sum(df$ID==id & df$k>k & df$choice==0, na.rm = T) + sum(df$ID==id & df$k<k & df$choice==1, na.rm = T))/(sum(!is.na(df$choice[df$ID==id]))-1)
  }
  best <- df %>% filter(ID==id & consistency == max(consistency[ID==id])) %>% select(k, consistency)
  df$k_sub[df$ID==id] <- geometric.mean(best$k)
  df$max_consistency[df$ID==id] <- max(best$consistency)
}
df$log_k_sub = log(df$k_sub)
sub_df4 <- df %>% select(ID, k_sub, log_k_sub, max_consistency, group) %>% unique()


