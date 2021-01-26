library(tidyverse)
library(modelr)
library(tidyverse)
library(lme4)
library(afex)
library(broom)
library(broom.mixed) #plays will with afex p-values in lmer wrapper
library(ggpubr)
library(car)
library(viridis)
library(psych)
library(corrplot)

# simulate dataset
sigmoid = function(x) {
  p = 1/(1+exp(-x))
  return(p)
}

# k = runif(1000, min = 0.001, max = 0.25)
# delay = runif(10000, min = 1, max = 300)
# sir = runif(10000, min = 1, max = 50)
# ldr = runif(10000, min = 50, max = 100)
# # compute variables
# dd <- as_tibble(data.frame(k, delay, sir, ldr))
# dd <- dd %>% mutate(ldr_disc = ldr/(1+k*delay),
#                     k_ind = (ldr/sir - 1)/delay,
#                     value_diff = ldr_disc - sir,
#                     value_ratio = ldr_disc/sir,
#                     p_ldr = 1/(1+exp(-value_diff)),
#                     p_ldr_mlm = 1/(1+exp(-(log(k_ind) - log(k)))),
#                     p_ldr_mlm_10 = 1/(1+exp(-(10*(log(k_ind) - log(k))))),
#                     p_ldr_mlm_5 = 1/(1+exp(-(5*(log(k_ind) - log(k))))),
#                     log_ratio = log(k_ind) - log(k),
#                     log_diff = log(k_ind) - log(k)
#                     )
# ggplot(dd, aes(value_diff, log_diff)) + geom_point()
# ggplot(dd, aes(k_ind, k_ind_simp)) + geom_point()
# cor.test(dd$value_diff,dd$log_diff)
# ggplot(dd, aes(value_diff, p_ldr_mlm, color = ldr, alpha = sir)) + geom_point() + geom_smooth()
ggplot(dd, aes(p_ldr, p_ldr_mlm_5, color = ldr, alpha = sir)) + geom_point() + geom_smooth()

mean(dd$value_diff)
plot(dd$value_diff,dd$p_ldr)
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
mcq=as_tibble(as.data.frame(MCQ_options))
names(mcq)=c('sir','ldr','delay')

ks <- runif(100, min = 0.001, max = 0.25)
mcq$k <- NA
dd <- mcq
for (k in ks) {
  temp <- mcq
  temp$k <- k
  dd <- rbind(mcq1,temp)
}
dd <- dd %>% mutate(ldr_disc = ldr/(1+k*delay),
                    k_ind = (ldr/sir - 1)/delay,
                    value_diff = ldr_disc - sir,
                    value_ratio = ldr_disc/sir,
                    p_ldr = 1/(1+exp(-value_diff)),
                    p_ldr_mlm = 1/(1+exp(-(log(k_ind) - log(k)))),
                    p_ldr_mlm_10 = 1/(1+exp(-(10*(log(k_ind) - log(k))))),
                    p_ldr_mlm_half = 1/(1+exp(-(.5*(log(k_ind) - log(k))))),
                    p_ldr_mlm_5 = 1/(1+exp(-(5*(log(k_ind) - log(k))))),
                    log_ratio = log(k_ind) - log(k),
                    log_diff = log(k_ind) - log(k)
)
# ggplot(dd, aes(value_diff, log_diff)) + geom_point()
# ggplot(dd, aes(k_ind, k_ind_simp)) + geom_point()
# cor.test(dd$value_diff,dd$log_diff)
# ggplot(dd, aes(value_diff, p_ldr_mlm, color = ldr, alpha = sir)) + geom_point() + geom_smooth()
# ggplot(dd, aes(p_ldr, p_ldr_mlm_5, color = ldr, alpha = sir)) + geom_point() + geom_smooth()
# ggplot(dd, aes(p_ldr, p_ldr_mlm_5, color = ldr, alpha = sir)) + geom_point() + geom_smooth(method = 'glm')
dt <- pivot_longer(dd, c(p_ldr_mlm, p_ldr_mlm_10, p_ldr_mlm_half, p_ldr_mlm_5, p_ldr_mlm_100), 
                   names_to = "beta", names_prefix = "p_ldr_mlm", values_to = "p_ldr_mlm")
ggplot(dt, aes(value_diff, p_ldr_mlm, color = sir)) + geom_smooth(method = 'gam') + geom_point() + 
  facet_wrap(~beta)
ggplot(dt, aes(p_ldr, p_ldr_mlm, color = sir)) + geom_smooth(method = 'gam') + geom_point() + 
  facet_wrap(~beta)

ggplot(dd, aes(value_diff, p_ldr_mlm, color = k)) + geom_smooth(method = 'gam')
ggplot(dd, aes(value_diff, p_ldr_mlm_5, color = k)) + geom_smooth(method = 'gam')
ggplot(dd, aes(value_diff, p_ldr_mlm_10, color = k)) + geom_smooth(method = 'gam')
ggplot(dd, aes(value_diff, p_ldr_mlm, color = k)) + geom_smooth(method = 'gam')
