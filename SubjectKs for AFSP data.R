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
# AFSP data, preprocessing
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
               site_code==3 ~ 'COL'))   %>% rename(subject = Subject)
adf <- merge(adf, sub_df[,c(1:121,485,497:505)], by = 'subject')

adf <- adf %>% mutate(delayMag_sc = scale(delayMag),
                      delay_sc = scale(delay),
                      immMag_sc = scale(immMag),
                      logk_sc = scale(logk))

setwd('~/OneDrive/papers/discounting/data/')
save(file = "discounting_processed_afsp.Rdata",list = ls(all = T))

# calculate subject-wise ks and consistencies

load('discounting_processed_afsp.Rdata')
df<-adf
df<-rename(df, id=subject)
df<-rename(df, ID=id)

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

sub_df <- df %>% select(ID, lethgrp, k_sub, log_k_sub, site_code) %>% unique()


setwd('~/OneDrive/papers/discounting/data/')
afsp_subs_with_sub_Ks<-sub_df

save(file = 'afsp_subs_with_sub_Ks.Rda', afsp_subs_with_sub_Ks)
write_sav(afsp_subs_with_sub_Ks, "afsp_subs_with_sub_Ks.sav")


####check on consistency, unlike max_consistency, it varies across ppt depending on MCQ question number
consistency_check<-df %>% select(ID, item, lethgrp, k_sub, log_k_sub, consistency, max_consistency, site_code)
