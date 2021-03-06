# Reads in behavioral data and clinical characteristics
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
library(haven)
setwd("~/OneDrive/papers/discounting/data")

pit <-FALSE
afsp <-  TRUE
if (pit) {
# Pittsburgh data
df <- as_tibble(read.csv("~/OneDrive/papers/discounting/data/MCQwithdemo.csv"))  %>% 
  dplyr::rename(immMag = Immediate.magnitude, 
                delayMag = Delayed.magnitude,
                delay = Lenth.of.delay,
                choice = Response,
                item = Item) %>% 
  mutate(magRatio = delayMag/immMag,
         logMagRatio = log(magRatio),
         k = (magRatio-1)/delay,
         logk = log(k))
design <- df %>% select(item, immMag, delayMag, delay, k, logk) %>% unique() %>% arrange(item) %>% mutate(item = as.integer(item))
save(file = 'mcq_design.Rdata', design)

df$Income[df$Income>10] <- NA
df$Education[df$Education>25] <- NA
df$wtar.score[df$wtar.score>200] <- NA
df <- df %>% mutate(choiceChar = case_when(
  choice==0 ~ 'now',
  choice==1 ~ 'later',
  choice==2 ~ 'NA'),
  # scale variables for regression models
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
  income_sc = scale(Income), 
  mmse_sc = scale(mmse.score))
df$choice[df$choice==2] <- NA


# calculate subject-wise ks and consistencies
df <- df %>% arrange(ID, k)
ks <- unique(df$k)
ids <- unique(df$ID)
df$consistency <- NA
df$k_sub <- NA
df$max_consistency <- NA
for (id in ids) {
  for (k in ks) {
    df$consistency[df$ID==id & df$k==k] = (sum(df$ID==id & df$k<k & df$choice==0, na.rm = T) + sum(df$ID==id & df$k>k & df$choice==1, na.rm = T))/sum(!is.na(df$choice[df$ID==id]))
  }
  df$k_sub[df$ID==id] <- geometric.mean(df$k[df$consistency==max(df$consistency[df$ID==id])])
  df$max_consistency[df$ID==id] <- max(df$consistency[df$ID==id])
}
df$log_k_sub = log(df$k_sub)
setwd('~/OneDrive/papers/discounting/data/')
save(file = "discounting_processed_pit.Rdata",list = ls(all = T))
}

if (afsp) {
# AFSP data
# load the design
  load('mcq_design.Rdata')
  
adf <- read_csv("~/OneDrive/papers/discounting/data/AFSP_MCQ_Merged_Comma.csv") %>% 
  gather(item, choice, MCQ1:MCQ30) %>% 
  mutate(item = substr(item, 4,5),
    item = as.integer(item),
    choice[choice>2] <- NA,
    choice = choice-1)  %>% rename(subject = Subject) %>% merge(design, by = 'item') 
sub_df <- read_spss('AFSP ALL SITES ALL MEASURES MERGED Vb1.4.sav')  %>% filter(use == 1) %>% 
  mutate(    site_code = as.factor(site_code),
             lethgrp = as.factor(lethgrp),
             groupLeth = case_when(
               lethgrp==3  ~ 'HL',
               lethgrp==2 ~ 'LL',
               lethgrp==1 ~ 'DEP',
               lethgrp==0 ~ 'HC'), 
             site = case_when(
               site_code==1 ~ 'NY',
               site_code==2 ~ 'PGH',
               site_code==3 ~ 'COL'))
adf <- merge(adf, sub_df[,c(1:121,485,497:505)], by = 'subject')

adf <- adf %>% mutate(delayMag_sc = scale(delayMag),
    delay_sc = scale(delay),
    immMag_sc = scale(immMag),
    logk_sc = scale(logk)) 
  

setwd('~/OneDrive/papers/discounting/data/')
save(file = "discounting_processed_afsp.Rdata",list = ls(all = T))
}


