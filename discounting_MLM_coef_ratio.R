#### improved MLMs to estimate kappa/beta, the revised preference parameter
# first run simulation_results_plots.R 

# rstan ----
library(rstan)
library(rstanarm)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

mb1 <- stan_glmer(choice ~ logk_sc * noise + logk_sc * log_k_true - 1  +
              (1|ID), family = binomial, ldf, chains = 10, iter = 4000)
summary(mb1)

#########################
#frequentist approach with mratios----

install.packages("mratios")
library(mratios)
data("BW")
boxplot(Weight~Dose, data = BW)
lmfit <- lm(Weight~Dose-1, data=BW)
est <- coefficients(lmfit)
vc <- vcov(lmfit)
CMAT <- contrMatRatio(table(BW$Dose), type = "Dunnett")
BWnoninf <- gsci.ratio(est, vc, CMAT$numC, CMAT$denC,
                       alternative="greater", degfree=lmfit$df.residual)

BWnoninf

# Plot
plot(BWnoninf, rho0=0.9)
library(lme4)
library(nlme)
data("Milk")
lmefit <- lmer(protein ~ Diet - 1 + (Time|Cow), data = Milk)
summary(lmefit)
vcov(lmefit)

# glmer on simulated data

m1 <- glmer(choice ~ logk_sc * noise + logk_sc * log_k_true - 1  +
              (1|ID), family = binomial, ldf, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
summary(m1)
while (any(grepl("failed to converge", m1@optinfo$conv$lme4$messages) )) {
  ss <- getME(m1,c("theta","fixef"))
  m1 <- update(m1, start=ss, control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap","bobyqa"),optCtr=list(maxfun=2e5)))}
summary(m1)


estm <- fixef(m1)
vcm <- vcov(m1)
CM <- rbind(c(0,0,0,0,0,0,0,0,0,0), # logk_sc
            c(0,1,0,0,0,0,0,0,0,0), # noise0
            c(0,0,1,0,0,0,0,0,0,0), # noise0.1
            c(0,0,0,1,0,0,0,0,0,0), # noise0.33
            c(0,0,0,0,1,0,0,0,0,0), # noise0.67
            c(0,0,0,0,0,1,0,0,0,0), # log_k_true-5.64
            c(0,0,0,0,0,0,0,0,0,0), # logk_sc:noise0.1
            c(0,0,0,0,0,0,0,0,0,0), # logk_sc:noise0.33
            c(0,0,0,0,0,0,0,0,0,0), # logk_sc:noise0.67 
            c(0,0,0,0,0,0,0,0,0,0) # logk_sc:log_k_true-5.64
            )

DM <- rbind(c(0,0,0,0,0,0,0,0,0,0), # logk_sc
            c(1,0,0,0,0,0,0,0,0,0), # noise0
            c(0,0,0,0,0,0,1,0,0,0), # noise0.1
            c(0,0,0,0,0,0,0,1,0,0), # noise0.33
            c(0,0,0,0,0,0,0,0,1,0), # noise0.67
            c(0,0,0,0,0,0,0,0,0,1), # log_k_true-5.64
            c(0,0,0,0,0,0,0,0,0,0), # logk_sc:noise0.1
            c(0,0,0,0,0,0,0,0,0,0), # logk_sc:noise0.33
            c(0,0,0,0,0,0,0,0,0,0), # logk_sc:noise0.67 
            c(0,0,0,0,0,0,0,0,0,0) # logk_sc:log_k_true-5.64
)

rownames(CM) <- c("logk_sc", "noise0", "noise0.1", "noise0.33", "noise0.67", "log_k_true-5.64", "logk_sc:noise0.1", "logk_sc:noise0.33", "logk_sc:noise0.67", "logk_sc:log_k_true-5.64")
gscimix <- gsci.ratio(estm, vcm, CM, DM)

chkcorr <- function(x) {
  if (!is.matrix(x) || (d <- dim(x))[1] != d[2])
    return(FALSE)
  rownames(x) <- colnames(x) <- NULL
  storage.mode(x) <- "numeric"
  ONE <- 1 + sqrt(.Machine$double.eps)
  
  ## return
  -ONE <= min(x) && max(x) <= ONE && isTRUE(all.equal(diag(x), rep(1, d[1])))
}

lmefit <- lme(protein ~ Diet-1, data=Milk,
              random=~Time|Cow, correlation=corAR1(form=~Time|Cow))

# Hirschberg approach
library(haven) # A package to read new Stata data sets
library(foreign) # A package to read Stata data sets
library("msm") # for calculation delta standard errors
library(ggplot2) # advanced graphics
library(lmtest) # testing linear regression models
library(systemfit) # estimation of sur models
library(AER) # Applied Econometrics with R
library(car) # companion to Applied Regression
library("plm") # linear models for panel data
library(sandwich) # different robust covariance matrix estimators 

# 3.1 Parallel-line assay: A compensation interpretation of a dummy variable.
#==================================================================================
# read txt file into data set fall
#
fall= read.dta("https://www.online.fbe.unimelb.edu.au/t_drive/FTS/data/new_fall.dta")
#
# Run a regression on the change in the change in EGM expenditures (fall)
# as a function of the dummy for the border (nborder),
# the change in the number of EGMs (cegm) and the measure of socioeconomic status
# of the population (irsed).
# save the results in sures
#
reg2 = (lm(fall~irsed+nborder+cegm,data=fall))
summary(reg2)
sures = summary(reg2)
#
# Use robust or White standard errors type 1
#
rreg2 = coeftest(reg2,vcov=hccm(reg2,type="hc1"))
#
# save df = degrees of freedom; B = estimated coefficients and the robust
# variance-covaiance matrix V
#
df = sures$df
b = sures$coefficients
B = b[,1]
V = vcovHC(reg2, type = "HC1")
#
# Choose the significance level and find the appropriate critical value
#
nalpha = 0.05
t2 = qt(1-nalpha/2, df[2])

#
# Define the transformation matrix for this case to pick the last two
# coefficients
#
T = matrix(0, nrow = 2, ncol = 4)
T[1,3] = 1
T[2,4] = 1

B2 = T%*%B
VV = (T%*%V)%*%t(T)

# Quadratic Solution for the Fieller
#
aa = (B2[2,1]^2) - (t2^2*VV[2,2])
bb = (2*t2^2*VV[1,2]) - (2*B2[1,1]*B2[2,1])
cc = (B2[1,1]^2) - (t2^2*VV[1,1])
rad = sqrt(bb^2 - 4*aa*cc)
lm1 = (-bb + rad ) / (2 * aa)
lm2 = (-bb - rad ) / (2 * aa)
message("Fieller Ratio = ", tp ,", Upper = ", lm1, ", Lower = ", lm2)
#
# Plot out p = the linear combination and its corresponding confidence
# bounds pu and pl for the graphical Fieller where n1 and n2 are the bounds
# of the possible values
#
n1 = -10
n2 = 40
x1 = seq(n1, n2, by = sign(n2-n1)*5)
x = matrix(x1,nrow=NROW(x1), ncol = 1)
p = B2[1,1] - B2[2,1]*x
vp = VV[1,1] - 2*VV[1,2]*x + VV[2,2]*x^2
pu = p + t2*sqrt(vp)
pl = p - t2*sqrt(vp)
y = cbind(p,pu,pl)
matplot(x,y,type='l',col="blue",main="Border Influence on Pokies")
abline(h = 0, col = "gray60")
abline(v = dflb, col = "black")
abline(v = dfub, col = "black")
abline(v = lm1, col = "red")
abline(v = lm2, col = "red")
abline(v = tp, col = "violet")
text(tp,10, "Ratio", col = "violet", adj = c(-.1, -.1))
