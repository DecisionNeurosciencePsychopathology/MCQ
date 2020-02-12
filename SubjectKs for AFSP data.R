df<-rename(df, id=subject)
df<-rename(df, ID=id)


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

sub_df <- df %>% select(ID, groupLeth, k_sub, log_k_sub, site) %>% unique()


setwd('~/OneDrive/papers/discounting/data/')
afsp_subs_with_sub_Ks<-sub_df

save(file = 'afsp_subs_with_sub_Ks.Rda', afsp_subs_with_sub_Ks)
library(haven)
write_sav(afsp_subs_with_sub_Ks, "afsp_subs_with_sub_KsUPD.sav")
####check on consistency, seems off/not creating 1 row per person


