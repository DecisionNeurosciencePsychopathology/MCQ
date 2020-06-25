library(bbmle)
library(lme4)
library(tidyr)
library(corrplot)

# source functions ####
# these are used to estimate k values- run them so R knows what they are
estimate_k=function(MCQ_options,MCQ_choices_grp) {
  num_subjs=dim(MCQ_choices_grp)[1]
  k_vals=exp(seq(-3,-7,by=-2)) #starting points for estimating k
  
  #preallocate output
  LL_matrix=matrix(data=NA,nrow=num_subjs,ncol=4)
  
  for (s in 1:num_subjects) {
    LL_subj=matrix(data=NA,nrow=length(k_vals),ncol=4)
    
    #loop through values to estimate from multiple starting points
    for (ind in 1:length(k_vals)) {
      in_data=cbind(MCQ_options,as.numeric(MCQ_choices_grp[s,]))
      names(in_data)=c(names(MCQ_options),'sooner_choice')
      
      mle_fit=bbmle::mle2(hyperbolic_k_estimate,start=list(k=k_vals[ind]),
                          method="Brent",data=in_data,lower=0,upper=0.2)
      if (exists('mle_fit')&&mle_fit@details$convergence==0) {
        iter_LL=as.numeric(logLik(mle_fit))
        iter_k=mle_fit@coef
        mle_fit_old=mle_fit
        rm(mle_fit)
      } else {
        iter_LL=NA
        iter_k=NA
      }
      
      #add to matrix: subj #, LL, k, starting k point
      LL_subj[ind,]=c(s,iter_LL,iter_k,k_vals[ind])
    }
    #get minimum -LL for subject and add that row's k estimate to output
    min_LL_ind=which(LL_subj[,2]==min(LL_subj[,2],na.rm=T))
    if (length(min_LL_ind)>1) min_LL_ind=min_LL_ind[1]
    LL_matrix[s,]=LL_subj[min_LL_ind,]
    
    
  }
  return(LL_matrix[,3])
  
}

hyperbolic_k_estimate=function(k) {
  num_choices=length(sooner_choice)
  P_chosen=array(data=NA,dim=num_choices)
  
  for (n in 1:num_choices) {
    disc_delay_val=delay_reward[n]*(1/(1+k*delay[n]))
    if (sooner_choice[n]==1) {
      #probability of choosing sooner reward is 1 if value is higher otherwise 0
      #needs to be slightly greater than 0, actually, to avoid log(0) issues
      P_chosen[n]=ifelse(imm_reward[n]>disc_delay_val,1,0.00000001)
    } else if (sooner_choice[n]==0) {
      P_chosen[n]=ifelse(imm_reward[n]<disc_delay_val,1,0.00000001)
    }
  }
  LL=-1*sum(log(P_chosen)) #negative log likelihood
  
  return(LL)
  
}

# set up variables ####
#log(k) values- take from group means in 2011 paper (using 1/number of days until delayed value is halved)
mean_logk=c(-5.64,-5.35,-4.5,-3.95,-6.23)
grp1_means=c(mean_logk[1],mean_logk[4])
grp2_means=c(mean_logk[1],mean_logk[4])

#SDs in values- this is the amount of noise
var_values=c(0,0.33,0.67,1) #noise is proportion of value
grp1_noise=var_values
grp2_noise=var_values

num_subjects=100 #number of subjects per group

all_sim_out=matrix(data=NA,nrow=length(grp1_means)*length(grp2_means)*
                    length(grp1_noise)*length(grp2_noise),ncol=10)

# set up choices from MCQ
#NOTE: please check this as I manually entered all the numbers!
#also, participants may have completed more ?s than the orig MCQ, 
# though that shouldn't affect the simulations much
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

# simulate choices for a given k and noise per group, looping through all combinations ####

for (a in 1:length(grp1_means)) {
  grp1_k=grp1_means[a]
  
  for (b in 1:length(grp2_means)) {
    grp2_k=grp2_means[b]
    
    for (c in 1:length(grp1_noise)) {
      noise1=grp1_noise[c]
      
      for (d in 1:length(grp2_noise)) {
        noise2=grp2_noise[d]
        
        # make data frame to store output for this set of simulated values
        MCQ_choices=array(data=NA,dim=c(num_subjects*2,30))
        
        #create non-noisy values for discounted future reward based on k
        #note: here, we can create an array of discounted values for all choices rather 
        # than looping, to make things faster
        discounted_values_grp1=MCQ_options$delay_reward*
          (1/(1+exp(grp1_k)*MCQ_options$delay))
        discounted_values_grp2=MCQ_options$delay_reward*
          (1/(1+exp(grp2_k)*MCQ_options$delay))
        
        #loop through subjects - this assumes each subject has a different value 
        # for immediate and discounted future rewards based on the overall noise
        for (s in 1:num_subjects) {
          
          #select a noisy immediate and delayed reward based on mean (non-noisy value)
          # and SD (given by us - proportion of value)
          #have to loop through choices because rnorm doesn't work on arrays
          for (q in 1:30) {
            #group 1
            noisy_imm_reward=rnorm(1,MCQ_options$imm_reward[q],noise1*MCQ_options$imm_reward[q])
            noisy_discounted_value=rnorm(1,discounted_values_grp1[q],noise1*discounted_values_grp1[q])

            MCQ_choices[s,q]=ifelse(noisy_imm_reward>noisy_discounted_value,1,0) #1 for imm, 0 for delayed
            
            #group 2
            noisy_imm_reward=rnorm(1,MCQ_options$imm_reward[q],noise2*MCQ_options$imm_reward[q])
            noisy_discounted_value=rnorm(1,discounted_values_grp2[q],noise2*discounted_values_grp2[q])

            MCQ_choices[(num_subjects+s),q]=ifelse(noisy_imm_reward>noisy_discounted_value,1,0) 
          }
        }
        
        MCQ_choices=data.frame(MCQ_choices)
        names(MCQ_choices)=paste0('Q',seq(1,30,by=1))
        
        # calculate k values per group- uses functions at top of script
        # this is a quick and dirty ML estimation so may be incorrect
        # after running this - definitely incorrect, don't use
        grp1_k_est=estimate_k(MCQ_options=MCQ_options,MCQ_choices_grp=MCQ_choices[1:num_subjects,])
        grp2_k_est=estimate_k(MCQ_options=MCQ_options,MCQ_choices_grp=
                                MCQ_choices[(1+num_subjects):(2*num_subjects),])
        
        #create question-level (long) data frame of simulated choices and add 
        # logk_sc (from MCQ_options), group, and ID variables
        MCQ_choices_long=pivot_longer(MCQ_choices,cols=names(MCQ_choices),names_to='Question')
        MCQ_choices_long$Question=as.numeric(sub('^Q','',MCQ_choices_long$Question))
        MCQ_choices_long$logk_sc=MCQ_options$logk_sc[MCQ_choices_long$Question]
        MCQ_choices_long$group=as.factor(c(rep(1,num_subjects*30),rep(2,num_subjects*30)))
        MCQ_choices_long$ID=c(rep(1:(2*num_subjects),each=30))
        
        #calculate effect of logk_sc by group
        grp_lme=glmer(value ~ logk_sc * group + (1|ID), family = binomial, MCQ_choices_long, 
            control=glmerControl(nAGQ0initStep=FALSE, optimizer = c("nloptwrap"),optCtr=list(maxfun=2e5)))
        lme_summary=summary(grp_lme)
        
        
        #put values in output
        count=32*(a-1)+16*(b-1)+4*(c-1)+d
        all_sim_out[count,]=c(grp1_k,grp2_k,noise1,noise2,median(grp1_k_est),
                              median(grp2_k_est),lme_summary$coefficients[,3])
        
        
      }
    }
  }
}

#test relationships between simulated differences and glmer effects ####
all_sim_out=data.frame(all_sim_out)
names(all_sim_out)=c('sim_k_grp1','sim_k_grp2','sim_noise_grp1','sim_noise_grp2',
                     'median_est_k_grp1','median_est_k_grp2','intercept_z',
                     'logk_z','group_z','logk_group_z')
all_sim_out$group_k_sim_diff=all_sim_out$sim_k_grp1-all_sim_out$sim_k_grp2
all_sim_out$group_k_sim_avg=rowMeans(cbind(all_sim_out$sim_k_grp1,all_sim_out$sim_k_grp2))
# all_sim_out$group_k_est_diff=all_sim_out$median_est_k_grp1-all_sim_out$median_est_k_grp2
all_sim_out$total_noise=all_sim_out$sim_noise_grp1+all_sim_out$sim_noise_grp2
all_sim_out$group_noise_diff=all_sim_out$sim_noise_grp1-all_sim_out$sim_noise_grp2

corrplot(cor(all_sim_out))

#intercept related to group 1 (reference group) k value
cor.test(all_sim_out$intercept_z,all_sim_out$sim_k_grp1) #sig. positive  SUPPLEMENTAL PLOT intercept--ref group k

#group main effect related to difference in groups' k values, less related
# to differences in noise
cor.test(all_sim_out$group_z,all_sim_out$group_k_sim_diff) #sig. negative    SUPPLEMENTAL PLOT group main effect -- true k difference 
cor.test(all_sim_out$group_z,all_sim_out$group_noise_diff) #somewhat positive

#group*k interaction related to difference in groups' noise levels, less
# related to differences in k values
cor.test(all_sim_out$logk_group_z,all_sim_out$group_noise_diff) #sig. negative SUPPLEMENTAL PLOT group*k -- true noise diff
cor.test(all_sim_out$logk_group_z,all_sim_out$group_k_sim_diff) #somewhat positive


###AT CORRELATIONS:

M <- all_sim_out %>% select(intercept_z, sim_k_grp1, group_z, group_k_sim_diff, logk_group_z, group_noise_diff)
K<-cor(M)
pdf('vanessa_simulation_corrplot.pdf', height = 8, width = 8)
corrplot(K, method="number")
dev.off()
