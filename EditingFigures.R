###CURVES

# Wes Anderson version
library(wesanderson)
setwd('~/OneDrive/papers/discounting/plots/')
##Pitt Non AFSP
non_afsp_subs_long <- rename(non_afsp_subs_long, Lethality = "groupLeth")
non_afsp_subs_long<-non_afsp_subs_long %>% mutate(Lethality = case_when(
  Lethality == 'HC' ~ 'Controls',
  Lethality == 'DEP' ~ 'MDD',
  Lethality == 'IDE' ~ 'SI + MDD',
  Lethality == 'LL' ~ 'LL SA + MDD',
  Lethality == 'HL' ~ "HL SA + MDD"
))
non_afsp_subs_long$Lethality <- factor(non_afsp_subs_long$Lethality, levels = c("Controls", "MDD", "SI + MDD", "LL SA + MDD", "HL SA + MDD"))

pal = wes_palette("Zissou1", type = "discrete")
pdf("discounting_choice_by_k_group_pit_non_afsp_wa.pdf", width = 4, height = 3.5)
ggplot(non_afsp_subs_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()

##Pitt AFSP

afsp_pit_long <- rename(afsp_pit_long, Lethality = "lethgrp")
afsp_pit_long<-afsp_pit_long %>% mutate(Lethality = case_when(
  Lethality == '0' ~ 'Controls',
  Lethality == '1' ~ 'MDD',
  Lethality == '2' ~ 'LL SA + MDD',
  Lethality == '3' ~ "HL SA + MDD"
))
afsp_pit_long$Lethality <- factor(afsp_pit_long$Lethality, levels = c("Controls", "MDD", "LL SA + MDD", "HL SA + MDD"))

pal = wes_palette("Zissou1", 4, type = "continuous")
pdf("discounting_choice_by_k_group_pit_afsp_wa.pdf", width = 4, height = 3.5)
ggplot(afsp_pit_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()






##Non-Pitt AFSP
afsp_non_pit_long <- rename(afsp_non_pit_long, Lethality = "lethgrp")
afsp_non_pit_long<-afsp_non_pit_long %>% mutate(Lethality = case_when(
  Lethality == '0' ~ 'Controls',
  Lethality == '1' ~ 'MDD',
  Lethality == '2' ~ 'LL SA + MDD',
  Lethality == '3' ~ "HL SA + MDD"
))
afsp_non_pit_long$Lethality <- factor(afsp_non_pit_long$Lethality, levels = c("Controls", "MDD", "LL SA + MDD", "HL SA + MDD"))

pal = wes_palette("Zissou1", 4, type = "continuous")
pdf("discounting_choice_by_k_group_non_pit_afsp_wa.pdf", width = 4, height = 3.5)
ggplot(afsp_non_pit_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()





####EM::MEANS
#####AFSP PITT
#####AFSP Pitt general model no covariates SAMPLE 2
m1 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|subject), family = binomial, afsp_pit_long, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)
car::Anova(m1)

setwd('~/OneDrive/papers/discounting/plots/')

pal = wes_palette("Zissou1", 4, type = "continuous")
em1 <- as_tibble(emmeans::emtrends(m1, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelmAFSPPitt.pdf", height = 6, width = 12)
ggplot(em1, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
scale_x_discrete(limits=c("0","1","2","3"), labels = c("Controls", "MDD", "LL SA+MDD", "HL SA+MDD")) +
scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()

####REGULAR PALETTE 

em1 <- as_tibble(emmeans::emtrends(m1, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelmAFSPPitt.pdf", height = 3, width = 6)
ggplot(em1, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("0","1","2","3"), labels = c("Controls", "MDD", "LL SA+MDD", "HL SA+MDD")) 
dev.off()

#####AFSP NON PITT
#####AFSP Non Pitt general model no covariates SAMPLE 3
m2 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + (1|subject), family = binomial, afsp_non_pit_long,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2)
while (any(grepl("failed to converge", m2@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2,c("theta","fixef"))
  m2 <- update(m2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m2)
Anova(m2, '3')
vif(m2)
car::Anova(m2)


pal = wes_palette("Zissou1", 4, type = "continuous")
em2 <- as_tibble(emmeans::emtrends(m2, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelmAFSPnonPitt.pdf", height = 6, width = 12)
ggplot(em2, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("0","1","2","3"), labels = c("Controls", "MDD", "LL SA+MDD", "HL SA+MDD")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()



####REGULAR PALETTE 
em2 <- as_tibble(emmeans::emtrends(m2, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelmAFSPnonPitt.pdf", height = 3, width = 6)
ggplot(em2, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("0","1","2","3"), labels = c("Controls", "MDD", "LL SA+MDD", "HL SA+MDD"))
dev.off()
######
#####Pitt general model no covariates SAMPLE 1
m3 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3)
while (any(grepl("failed to converge", m3@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3,c("theta","fixef"))
  m3 <- update(m3, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m3)
Anova(m3, '3')
vif(m3)
car::Anova(m3)

pal = wes_palette("Zissou1", 5, type = "continuous")
em3 <- as_tibble(emmeans::emtrends(m3, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelmNonAFSPPitt.pdf", height = 6, width = 12)
ggplot(em3, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("HC","DEP","IDE","LL", "HL"), labels = c("Controls","MDD","SI+MDD","LL SA + MDD", "HL SA+MDD")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))
dev.off()

####REGULAR PALETTE 

em3 <- as_tibble(emmeans::emtrends(m3, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
pdf("modelmNonAFSPPitt.pdf", height = 3, width = 6)
ggplot(em3, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("HC","DEP","IDE","LL", "HL"), labels = c("Controls","MDD","SI+MDD","LL SA + MDD", "HL SA+MDD"))
dev.off()


#########Combine all into single panel
##############################
##########################################
# Wes Anderson version
library(wesanderson)
setwd('~/OneDrive/papers/discounting/plots/')
##Pitt Non AFSP
non_afsp_subs_long <- rename(non_afsp_subs_long, Lethality = "groupLeth")
non_afsp_subs_long<-non_afsp_subs_long %>% mutate(Lethality = case_when(
  Lethality == 'HC' ~ 'Controls',
  Lethality == 'DEP' ~ 'MDD',
  Lethality == 'IDE' ~ 'SI + MDD',
  Lethality == 'LL' ~ 'LL SA + MDD',
  Lethality == 'HL' ~ "HL SA + MDD"
))
non_afsp_subs_long$Lethality <- factor(non_afsp_subs_long$Lethality, levels = c("Controls", "MDD", "SI + MDD", "LL SA + MDD", "HL SA + MDD"))

pal = wes_palette("Zissou1", type = "discrete")
a2<-ggplot(non_afsp_subs_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))

##Pitt AFSP

afsp_pit_long <- rename(afsp_pit_long, Lethality = "lethgrp")
afsp_pit_long<-afsp_pit_long %>% mutate(Lethality = case_when(
  Lethality == '0' ~ 'Controls',
  Lethality == '1' ~ 'MDD',
  Lethality == '2' ~ 'LL SA + MDD',
  Lethality == '3' ~ "HL SA + MDD"
))
afsp_pit_long$Lethality <- factor(afsp_pit_long$Lethality, levels = c("Controls", "MDD", "LL SA + MDD", "HL SA + MDD"))

pal = wes_palette("Zissou1", 4, type = "continuous")

a3<-ggplot(afsp_pit_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))







##Non-Pitt AFSP
afsp_non_pit_long <- rename(afsp_non_pit_long, Lethality = "lethgrp")
afsp_non_pit_long<-afsp_non_pit_long %>% mutate(Lethality = case_when(
  Lethality == '0' ~ 'Controls',
  Lethality == '1' ~ 'MDD',
  Lethality == '2' ~ 'LL SA + MDD',
  Lethality == '3' ~ "HL SA + MDD"
))
afsp_non_pit_long$Lethality <- factor(afsp_non_pit_long$Lethality, levels = c("Controls", "MDD", "LL SA + MDD", "HL SA + MDD"))

pal = wes_palette("Zissou1", 4, type = "continuous")
a4<-ggplot(afsp_non_pit_long, aes(log(k), choice, color = Lethality)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  scale_color_manual(values = pal) + 
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))






####EM::MEANS
#####AFSP PITT
#####AFSP Pitt general model no covariates SAMPLE 2
m1 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|subject), family = binomial, afsp_pit_long, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)
Anova(m1, '3')
vif(m1)
car::Anova(m1)

setwd('~/OneDrive/papers/discounting/plots/')

####REGULAR PALETTE 

em1 <- as_tibble(emmeans::emtrends(m1, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
b3<-ggplot(em1, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("0","1","2","3"), labels = c("Controls", "MDD", "LL SA+MDD", "HL SA+MDD")) 


#####AFSP NON PITT
#####AFSP Non Pitt general model no covariates SAMPLE 3
m2 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + logk_sc * site_code + (1|subject), family = binomial, afsp_non_pit_long,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m2)
while (any(grepl("failed to converge", m2@optinfo$conv$lme4$messages) )) {
  ss <- getME(m2,c("theta","fixef"))
  m2 <- update(m2, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m2)
Anova(m2, '3')
vif(m2)
car::Anova(m2)

####REGULAR PALETTE 
em2 <- as_tibble(emmeans::emtrends(m2, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
b4<-ggplot(em2, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("0","1","2","3"), labels = c("Controls", "MDD", "LL SA+MDD", "HL SA+MDD"))


######
#####Pitt general model no covariates SAMPLE 1
m3 <- glmer(choice ~ logk_sc * lethgrp_ref_hl + (1|ID), family = binomial, non_afsp_subs_long,  control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m3)
while (any(grepl("failed to converge", m3@optinfo$conv$lme4$messages) )) {
  ss <- getME(m3,c("theta","fixef"))
  m3 <- update(m3, start=ss, control=glmerControl(optimizer = "bobyqa",optCtr=list(maxfun=2e5)))}
summary(m3)
Anova(m3, '3')
vif(m3)
car::Anova(m3)

####REGULAR PALETTE 

em3 <- as_tibble(emmeans::emtrends(m3, var = 'logk_sc', specs = 'lethgrp_ref_hl'))
b2<-ggplot(em3, aes(lethgrp_ref_hl, logk_sc.trend)) + geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position=position_dodge(width=0.5)) + 
  geom_line(position=position_dodge(width=0.5)) + geom_point(position=position_dodge(width=0.5)) + labs(y="", x = "") +
  scale_x_discrete(limits=c("HC","DEP","IDE","LL", "HL"), labels = c("Controls","MDD","SI+MDD","LL SA + MDD", "HL SA+MDD"))



#######Sample Graphs########
########THREE IN ONE GRAPH FOR PREFERENCE
#####shift in the location of the curve as a reflection of preference, change 5: 
####goes right if change from 5 to 2 and goes left if change from 5 to 8
k<- (-100:-20)/10
df <- as_data_frame(x=c(k))
df$k <- df$value
df <- df %>% select (-value)


df$q <- 1/(1+exp(-1*(df$k + 7)))
df$q1 <- 1/(1+exp(-1*(df$k + 5)))
df$q2 <-1/(1+exp(-1*(df$k + 3)))

df<-as_tibble(df)
df<-df%>%pivot_longer(-k)


df<-df %>% mutate(name = case_when(
  name== 'q' ~ 'Preference 7',
  name=='q1' ~ 'Preference 5',
  name=='q2' ~ 'Preference 3'
))

pal = wes_palette("Zissou1", 2, type = "continuous")
a1<-ggplot(df, aes(k, value, color = name)) + geom_line()  + xlab("logk") + ylab("choice") + 
  labs(color="Preference") +
  
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))

a1

######################VALUE SENSITIVITY

k<- (-100:-20)/10
df_p <- as_data_frame(x=c(k))
df_p$k <- df_p$value
df_p <- df_p %>% select (-value)

#####steepness shift is value sensitivity
###example of a highly decisive plot, sensitivity parameter of -1
df_p$p <- 1/(1+exp(-1*(df_p$k + 5)))
##example of a highly indecisive plot, sensitivity parameter of .1
df_p$p1 <- 1/(1 + exp(-.1*(df_p$k + 5)))
df_p<-as_tibble(df_p)
df_p<-df_p%>%pivot_longer(-k)
df_p<-df_p %>% mutate(name = case_when(
  name== 'p' ~ 'High Value Sensitivity',
  name=='p1' ~ 'Low Value Sensitivity'
))

pal = wes_palette("Zissou1", 2, type = "continuous")
b1<-ggplot(df_p, aes(k, value, color = name)) + geom_line()  + xlab("logk") + ylab("choice") + 
  labs(color="Value Sensitivity") +
  
  theme(panel.grid.major = element_line(colour = "grey45"), 
        panel.grid.minor = element_line(colour = "grey45"), 
        panel.background = element_rect(fill = 'grey40'))

b1

####WITH LABELS

ggarrange(a2, b2, a3, b3, a4, b4, ncol=2, nrow = 3, 
labels = c('A2', 'B2', 'A3', 'B3', 'A4', 'B4'))


###WITHOUT LABELS
ggarrange(a1, b1, a2, b2, a3, b3, a4, b4, ncol=2, nrow = 4)


####SAVE AS PDF

setwd('~/OneDrive/papers/discounting/plots/')
pdf("Mega Figure", height = 6, width = 12)
ggarrange(a1, b1, a2, b2, a3, b3, a4, b4, ncol=2, nrow = 4)
dev.off()