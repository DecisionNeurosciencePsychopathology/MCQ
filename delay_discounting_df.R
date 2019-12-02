##library  
  library(tidyr)
  library(dplyr)
  library(lubridate)
  library(eeptools)
  library(na.tools)
  ##functions
    #startup
    startup()
    #ID map and master demo map
     md<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T,online = F,batch_size=1000L)
     idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
     names(idmap)<-c("masterdemoid","wpicid","soloffid")
    #p2 map
     p2<-bsrc.checkdatabase2(protocol=ptcs$protect, online=F, batch_size=1000L)
     #Fix id (if old id), then match dates of redcap/access to closest date to MCQ
     date.match<-function(x,y=MCQwdemo, id, cutoff){
       if(!is.Date(x$date)){as.Date(mdy(x$date))->x$date}
       if(id){x<-bsrc.findid(x,idmap = idmap,id.var = "ID")
       x[which(x$ifexist),]->x}
       y$CDATE[match(x$masterdemoid, y$masterdemoid)]->x$MCQdate
       x[which(!is.na(x$MCQdate)),]->x
       mutate(x, datedif=MCQdate-date)->x
       x$datedif<-as.numeric(x$datedif)
       xb<-do.call(rbind,lapply(split(x,x$masterdemoid),function(xa){
         xa[which.min(abs(as.numeric(xa$datedif))),]
       }))
       xb[which(abs(as.numeric(xb$datedif))<cutoff),]->xc
       return(xc)
     }
     
     date.match.ssi<-function(x,y=MCQwdemo, id, cutoff){
       if(!is.Date(x$date)){as.Date(mdy(x$date))->x$date}
       if(id){x<-bsrc.findid(x,idmap = idmap,id.var = "ID")
       x[which(x$ifexist),]->x}
       y$consentdate[match(x$masterdemoid, y$masterdemoid)]->x$consentdate
       x[which(!is.na(x$consentdate)),]->x
       mutate(x, datedif=consentdate-date)->x
       x$datedif<-as.numeric(x$datedif)
       xb<-do.call(rbind,lapply(split(x,x$masterdemoid),function(xa){
         xa[which.min(abs(as.numeric(xa$datedif))),]
       }))
       xb[which(abs(as.numeric(xb$datedif))<cutoff),]->xc
       return(xc)
     }
     
     date.match.sis<-function(x,y=MCQwdemo, id, cutoff){
       if(!is.Date(x$date)){as.Date(mdy(x$date))->x$date}
       if(id){x<-bsrc.findid(x,idmap = idmap,id.var = "ID")
       x[which(x$ifexist),]->x}
       y$mldate[match(x$masterdemoid, y$masterdemoid)]->x$mldate
       y$consentdate[match(x$masterdemoid, y$masterdemoid)]->x$consentdate
       x[which(!is.na(x$mldate)),]->x
       mutate(x, datedif=mldate-date)->x
       x$datedif<-as.numeric(x$datedif)
       xb<-do.call(rbind,lapply(split(x,x$masterdemoid),function(xa){
         xa[which.min(as.numeric(xa$datedif)),]
       }))
       xb %>% mutate(bl=ifelse(xb$mldate<xb$consentdate, 1, 0))->xb
       xb[which(xb$bl==1 | abs(as.numeric(xb$datedif))<cutoff),]->xc
       return(xc)
       }
#Grab demo info from redcap
  demo<-data.frame(registration_redcapid=md$data$registration_redcapid, P2condate=md$data$reg_condate_protect2, 
           P1condate=md$data$reg_condate_protect, S2condate=md$data$reg_condate_suicid2,
           S1condate=md$data$reg_condate_suicide, DOB=md$data$registration_dob,
           group=md$data$registration_group, gender=md$data$registration_gender,
           '1'=md$data$registration_race___1, '2'=md$data$registration_race___2,
           '3'=md$data$registration_race___3, '4'=md$data$registration_race___4,
           '5'=md$data$registration_race___5, na=md$data$registration_race___999,
           ethnicity=md$data$registration_hispanic, edu=md$data$registration_edu,
           marital=md$data$registration_marrs)
  #IDmap
  demo<-bsrc.findid(demo,idmap = idmap,id.var = "registration_redcapid")
  #fix race (X6=multiple races)
  demo %>% gather(key="race", value = "value", -c(masterdemoid, P2condate, P1condate, 
           S1condate, S2condate, DOB, group, gender, edu, ethnicity, marital))->demo
  demo %>% filter(value==1)->demo
  demo %>% filter(race!="ifexist")->demo
  demo[which(duplicated(demo$masterdemoid)),"masterdemoid"]->mraceid
  demo[which(demo$masterdemoid %in% mraceid),"race"]<-"X6"
  #fix consent date
  as.Date(demo$P2condate)->demo$P2condate
  as.Date(ymd(demo$S2condate))->demo$S2condate
  as.Date(ymd(demo$P1condate))->demo$P1condate
  as.Date(ymd(demo$S1condate))->demo$S1condate
  demo %>% mutate(consentdate=pmin(demo$S1condate, demo$S2condate, demo$P2condate, 
                demo$P1condate, na.rm=T))->demo
  demo[-c(1:4,13)]->demo
  demo[-which(is.na(demo$consentdate)),]->demo

#MCQdata  
  #Access MCQ
    MCQold<-read.csv(file ="C:/Users/buerkem/OneDrive - UPMC/Desktop/MCQ data/Copy of subjvalues_k.csv")
    #Change IDs on Access MCQ
    MCQold<-bsrc.findid(MCQold,idmap = idmap,id.var = "ï..ID")
    MCQold[-c(1,9:12)]->MCQold
    #Fix old data
    mdy(MCQold$CDATE)->MCQold$CDATE
    demo$consentdate[match(MCQold$masterdemoid, demo$masterdemoid)]->MCQold$consentdate
    MCQold %>% group_by(masterdemoid) %>% mutate(date_dif=CDATE-consentdate)->MCQold
    MCQold %>% group_by(masterdemoid) %>% filter(date_dif==min(date_dif)) %>%ungroup()->MCQold
    unique(MCQold[which(MCQold$date_dif>365),"masterdemoid"])->weirdids
    #MCQold %>% mutate(newvar=paste0(CDATE,masterdemoid))->MCQolds
    #MCQolds[which(!duplicated(MCQolds$newvar)),]->MCQolds
    #as.data.frame(table(MCQolds$masterdemoid)) %>% filter(Freq>1)->MCQoldsnum
    
  #Redcap MCQ
    MCQnew<-p2$data[c("registration_redcapid","redcap_event_name","bq_date","mcq_1","mcq_2","mcq_3", "mcq_4", "mcq_5", "mcq_6",
          "mcq_7","mcq_8","mcq_9","mcq_10","mcq_11","mcq_12","mcq_13","mcq_14",
          "mcq_15","mcq_16","mcq_17","mcq_18","mcq_19","mcq_20","mcq_21",
          "mcq_22","mcq_23","mcq_24","mcq_25","mcq_26","mcq_27","mcq_28","mcq_29","mcq_30")]
    MCQnew %>% filter(redcap_event_name=="baseline_arm_2")->MCQnew
    #Figure out MCQnew ids
    MCQnew<-bsrc.findid(MCQnew,idmap = idmap,id.var = "registration_redcapid")
    MCQnew[-c(1, 2, 35:38)]->MCQnew
    #Only grab new MCQ data from new dataset
    MCQnew[which(!MCQnew$masterdemoid %in% MCQold$masterdemoid),]->MCQnewunique
    #remove missingness
    MCQnewunique[-which(rowMeans(is.na(MCQnewunique))>0.5),]->MCQnewunique
    #Change names of variables
    names(MCQnewunique)<-paste("",gsub("mcq_","",names(MCQnewunique)))
    #Gather data
    gather(MCQnewunique, key="Item", value = "Response",-` masterdemoid`,-` bq_date`)->MCQgather
    #Match item level characteristics to new data
    items<-MCQold[1:30,2:5]
    as.numeric(MCQgather$Item)->MCQgather$Item
    merge(MCQgather, items, by="Item")->MCQgather
    MCQgather$` masterdemoid`->MCQgather$masterdemoid
    ymd(MCQgather$` bq_date`)->MCQgather$CDATE
    MCQgather[-c(2,3)]->MCQgather
  #Merge
  merge(MCQgather, MCQold, all=T)->finalMCQ
  #Remove terms
  terms<-data.frame(ID=md$data$registration_redcapid, P2condate=md$data$reg_condate_protect2, 
           P1condate=md$data$reg_condate_protect, S2condate=md$data$reg_condate_suicid2,
           S1condate=md$data$reg_condate_suicide, excludep2=md$data$reg_term_excl_protect2,
           excludep1=md$data$reg_term_excl_protect, excludes2=md$data$reg_term_excl_suicid2,
           excludes1=md$data$reg_term_excl_suicide, termp2=md$data$reg_term_reason_protect2,
           termp1=md$data$reg_term_reason_protect, terms2=md$data$reg_term_reason_suicid2,
           terms1=md$data$reg_term_reason_suicide)
  terms %>% filter(P2condate!=""| P1condate!="" |S2condate!=""|S1condate!="")->terms
  terms %>% filter(excludep2==1 | excludep1==1 | excludes2==1 |excludes1==1|termp2==3|
                   termp1==3 | terms2==3 | terms1==3)->terms
  terms<-bsrc.findid(terms,idmap = idmap,id.var = "ID")
  terms[which(terms$masterdemoid %in% unique(finalMCQ[which(finalMCQ$masterdemoid %in% terms$masterdemoid),"masterdemoid"])),]->MCQterms
  finalMCQ[which(!finalMCQ$masterdemoid %in% MCQterms$masterdemoid),]->finalMCQ
  
  
#Add demo info
  demo[which(demo$masterdemoid %in% finalMCQ$masterdemoid),]->demo
  finalMCQ$consentdate<-NULL
  merge(demo, finalMCQ, by="masterdemoid")->MCQwdemo
#Age at consent date (Must be 50+)
  as.Date(MCQwdemo$consentdate)->MCQwdemo$consentdate
  as.Date(MCQwdemo$DOB)->MCQwdemo$DOB
  MCQwdemo %>% mutate(bl.age=age_calc(DOB,enddate = consentdate, units="years", precise=F))->MCQwdemo
  MCQwdemo[which(MCQwdemo$bl.age>49),]->MCQwdemo
  
  
  
#Household income (from Macarthur SDQ)
  #Redcap
  income<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$bq_date,
                     Event=p2$data$redcap_event_name, Income=p2$data$macarthur_6)
  income[which(income$Event=="baseline_arm_2"),]->income
  income[which(!is.na(income$Income)),]->income
  as.character(income$ID)->income$ID
  as.Date(income$date)->income$date
  date.match(x=income,y=MCQwdemo, id=T, cutoff=99999)->income
  income[-c(1,3,6:11)]->income
  #Access
  Macarthur<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_SES.csv")
  Macarthur[c(1, 2, 19)]->Macarthur
  Macarthur$CDATE->Macarthur$date
  recode(Macarthur$Q6, "0"=1, "1"=2, "2"=3, "3"=4, "4"=5, "5"=6, "6"=7, "7"=8, "8"=9,"9"=998)->Macarthur$Q6
  na.rm(Macarthur)->Macarthur
  date.match(x=Macarthur, id=T, cutoff=99999)->Macarthur
  #Combine
  Macarthur<-data.frame(masterdemoid=Macarthur$masterdemoid, date=Macarthur$date, Income=Macarthur$Q6,stringsAsFactors = F)
  rbind(Macarthur,income)->Incomedf
  date.match(x=Incomedf, id=F, cutoff=99999)->Incomedf
  Incomedf$Income[match(MCQwdemo$masterdemoid, Incomedf$masterdemoid)]->MCQwdemo$income

  
#Baseline SSI
  #Redcap
  ssi<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$bq_date,
                     Event=p2$data$redcap_event_name, score=p2$data$ssi_s_worst)
  ssi[which(ssi$Event=="baseline_arm_2"),]->ssi
  ssi[which(!is.na(ssi$score)),]->ssi
  as.character(ssi$ID)->ssi$ID
  as.Date(ssi$date)->ssi$date
  date.match.ssi(x=ssi,y=MCQwdemo, id=T, cutoff=365)->ssi
  ssi[c(2,4,5)]->ssi
  #Access
    ssiold1<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_SIS_1.csv")
    ssiold2<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_SIS_2.csv")
    ssiold1$CDATE->ssiold1$date
    ssiold2$CDATE->ssiold2$date
    date.match.ssi(x=ssiold1,y=MCQwdemo, id=T, cutoff=365)->ssiold1
    date.match.ssi(x=ssiold2,y=MCQwdemo, id=T, cutoff=365)->ssiold2
    #worst at BL
    ssiold1[which(ssiold1$TIMEFRM==1),]->ssiold1
    ssiold2[which(ssiold2$TIMEFRM==1),]->ssiold2
    ssiold1[c(7:11,17,18)]->ssiold1
    ssiold2[c(5:18, 24, 25)]->ssiold2
    merge(ssiold1, ssiold2, by="masterdemoid")->ssiold
    #removed one person for ssi dates being too mismatched
  ssiold[which(ssiold$Q6==5),"Q6"]<-NA
  ssiold[which(ssiold$Q7==5),"Q7"]<-NA
  ssiold[which(ssiold$Q8==5),"Q8"]<-NA
  ssiold[which(ssiold$Q9==5),"Q9"]<-NA
  ssiold[which(ssiold$Q10==5),"Q10"]<-NA
  ssiold[which(ssiold$Q11==5),"Q11"]<-NA
  ssiold[which(ssiold$Q12==5),"Q12"]<-NA
  ssiold[which(ssiold$Q13==5),"Q13"]<-NA
  ssiold[which(ssiold$Q14==5),"Q14"]<-NA
  ssiold[which(ssiold$Q15==5),"Q15"]<-NA
  ssiold[which(ssiold$Q16==5),"Q16"]<-NA
  ssiold[which(ssiold$Q17==5),"Q17"]<-NA
  ssiold[which(ssiold$Q18==5),"Q18"]<-NA
  ssiold[which(ssiold$Q19==5),"Q19"]<-NA
  ssiold$score=ifelse(ssiold$Q1==0 & (ssiold$Q2==0 |  ssiold$Q2==1) & ssiold$Q3==0 & ssiold$Q4==0, 0, rowSums(ssiold[c(2:6,8:21)]))
  ssiold2<-data.frame(masterdemoid=ssiold$masterdemoid, date=ssiold$date.x, score=ssiold$score)
  #combine
  rbind(ssiold2,ssi)->SSI
  date.match.ssi(x=SSI, id=F, cutoff=365)->SSI
  SSI$score[match(MCQwdemo$masterdemoid, SSI$masterdemoid)]->MCQwdemo$bl.SSIscore
  
#MMSE 
  #Redcap
  mmse<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$mmse_date, score=p2$data$mmse_s_adj)
  mmse[which(!is.na(mmse$score)),]->mmse
  as.character(mmse$ID)->mmse$ID
  as.Date(mmse$date)->mmse$date
  date.match(x=mmse,y=MCQwdemo, id=T, cutoff=547.5)->mmse
  mmse[-c(1,5:10)]->mmse
  #Access
  mmseold<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/MMSE.csv")
  mmseold[c(1,2,33)]->mmseold
  na.rm(mmseold)->mmseold
  mmseold$CDATE->mmseold$date
  date.match(x=mmseold, id=T, cutoff=547.5)->mmseold
  #Combine
  mmseold<-data.frame(masterdemoid=mmseold$masterdemoid, date=mmseold$date, score=mmseold$ADRCTOTAL, stringsAsFactors = F)
  rbind(mmseold,mmse)->MMSE
  date.match(x=MMSE, id=F, cutoff=547.5)->MMSE
  MMSE$score[match(MCQwdemo$masterdemoid, MMSE$masterdemoid)]->MCQwdemo$mmse_score
  
  
#DRS
  #Redcap
  drs<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$drs_date, score=p2$data$drs_total)
  drs[which(!is.na(drs$score)),]->drs
  as.character(drs$ID)->drs$ID
  as.Date(drs$date)->drs$date
  date.match(x=drs,y=MCQwdemo, id=T, cutoff=547.5)->drs
  drs[c(2:4)]->drs
  #Access
  drsold<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_MDRS_S.csv")
  drsold[c(1,2,9)]->drsold
  drsold$CDATE->drsold$date
  date.match(x=drsold, id=T, cutoff=547.5)->drsold
  #Combine
  drsold<-data.frame(masterdemoid=drsold$masterdemoid, date=drsold$date, score=drsold$TOTA_MDRS, stringsAsFactors = F)
  rbind(drsold,drs)->DRS
  date.match(x=DRS, id=F, cutoff=547.5)->DRS
  DRS$score[match(MCQwdemo$masterdemoid, DRS$masterdemoid)]->MCQwdemo$drs_score
  
#WTAR 
  #Redcap
  wtar<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$wtar_date, score=p2$data$wtar_s_adj)
  wtar[which(!is.na(wtar$score)),]->wtar
  as.character(wtar$ID)->wtar$ID
  as.Date(wtar$date)->wtar$date
  date.match(x=wtar,y=MCQwdemo, id=T, cutoff=9999999)->wtar
  wtar[c(2:4)]->wtar
  #Access
  wtarold<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/080519_WTAR_Export.csv")
  wtarold[c(1,2,7)]->wtarold
  wtarold[which(!is.na(wtarold$WTARSS)),]->wtarold
  wtarold$CDATE->wtarold$date
  date.match(x=wtarold, id=T, cutoff=9999999)->wtarold
  #Combine
  wtarold<-data.frame(masterdemoid=wtarold$masterdemoid, date=wtarold$date, score=wtarold$WTARSS, stringsAsFactors = F)
  rbind(wtarold,wtar)->WTAR
  date.match(x=WTAR, id=F, cutoff=9999999)->WTAR
  WTAR$score[match(MCQwdemo$masterdemoid, WTAR$masterdemoid)]->MCQwdemo$wtar_score
  
#EXIT
    #Redcap
    exit<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$exit_date, score=p2$data$exit_total)
    as.character(exit$ID)->exit$ID
    exit<-bsrc.findid(exit,idmap = idmap,id.var = "ID")
    exit[which(!is.na(exit$score)),]->exit
    as.Date(exit$date)->exit$date
    exit[-c(1,5:8)]->exit
    #Access
    exit2<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_EXIT_S.csv")
    exit2[-c(3:15)]->exit2
    #any scores over 97 means NA was involved, remove from total
    exit2[which(exit2$EXITtot>97),"EXITtot"]<-NA
    exit2[which(!is.na(exit2$EXITtot)),]->exit2
    exit2$CDate->exit2$date
    date.match(exit2, id=T, cutoff=550)->exit2
    #fix dates after date match
    as.character(exit2$date)->exit2$date
    as.character(exit$date)->exit$date
    #Make new df
    exitold<-data.frame(masterdemoid=exit2$masterdemoid, date=exit2$date, score=exit2$EXITtot,stringsAsFactors = F)
    rbind(exit,exitold)->EXIT
    as.Date(EXIT$date) -> EXIT$date
    date.match(x=EXIT, id=F, cutoff = 547.5)->EXIT
    #merge into demo
    EXIT$score[match(MCQwdemo$masterdemoid, EXIT$masterdemoid)]->MCQwdemo$exit_score
  
#HAM
  #Redcap
    ham<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$ham_date, ham1=p2$data$ham_1_dm,
                  ham2=p2$data$ham_2_gf, ham3=p2$data$ham_3_su, ham4=p2$data$ham_4_ii, ham5=p2$data$ham_5_im,
                  ham6=p2$data$ham_6_di, ham7=p2$data$ham_7_wi, ham8=p2$data$ham_8_re,
                  ham9=p2$data$ham_9_ag, ham10=p2$data$ham_10_psya, ham11=p2$data$ham_11_soma,
                  ham12=p2$data$ham_12_gi, ham13=p2$data$ham_13_gs, ham14=p2$data$ham_14_sex,
                  ham15=p2$data$ham_15_hd,ham16=p2$data$ham_16_li, ham17=p2$data$ham_17_weight,
                  ham18=p2$data$ham_18_rt, ham19=p2$data$ham_19_dp,ham20=p2$data$ham_20_prsx,
                  ham21=p2$data$ham_21_ocsx,ham22=p2$data$ham_22_xhelp, ham23=p2$data$ham_23_xhope,
                  ham24=p2$data$ham_24_xworth)
    rowSums(ham[3:19])->ham$ham17score
    rowSums(ham[3:26])->ham$ham24score
    ham[which(!is.na(ham$ham17score) | !is.na(ham$ham24score)),]->ham
    as.character(ham$ID)->ham$ID
    as.Date(ymd(ham$date))->ham$date
    date.match(x=ham,y=MCQwdemo, id=T, cutoff=30)->ham
    ham[c(2,27:29)]->ham
  #Access
    ham17old<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_HRSD.csv")
    ham24old<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_HRSD_24.csv")
    rowSums(ham17old[5:21])->ham17old$ham17score
    rowSums(ham24old[5:34])->ham24old$ham24score
    rowSums(ham24old[5:26])->ham24old$ham17score
    ham17old[c(1,2,27)]->ham17old
    ham24old[c(1,2,41,42)]->ham24old
    na.rm(ham17old)->ham17old
    ham24old[which(!is.na(ham24old$ham24score) | !is.na(ham24old$ham17score)),]->ham24old
    mdy(ham17old$CDATE)->ham17old$date
    mdy(ham24old$CDATE)->ham24old$date
    date.match(x=ham17old, id=T, cutoff=30)->ham17old
    date.match(x=ham24old, id=T, cutoff=30)->ham24old
    ham17old[c(3,4,5)]->ham17old
    ham24old[c(3,4,5,6)]->ham24old
    merge(ham17old,ham24old, by=c("masterdemoid","date","ham17score"), all=T)->hamold
  ###########LEFT OFF ON HAM
  hamold2<-data.frame(masterdemoid=hamold$masterdemoid, date=hamold$date, ham17score=hamold$ham17score,
                      ham24score= hamold$ham24score, stringsAsFactors = F)
    rbind(hamold2,ham)->HAM
    as.Date(HAM$date) -> HAM$date
    date.match(x=HAM, id=F, cutoff = 30)->HAM
    #merge into demo
    HAM$ham17score[match(MCQwdemo$masterdemoid, HAM$masterdemoid)]->MCQwdemo$ham17score
    HAM$ham24score[match(MCQwdemo$masterdemoid, HAM$masterdemoid)]->MCQwdemo$ham24score
    
    
###Redcap
      bsrc.getSUIHX_index<-function(protocol=protocol.cur,suicide_formname="suicide_history"){
      metadata<-bsrc.getform(protocol = protocol,formname = suicide_formname,aggressivecog = F,mod = F,grabnewinfo = F,batch_size=50L)
      sui_names<-names(metadata)
      index_df<-data.frame(names=sui_names,rxsim1=gsub(".*_(at[0-9]*$)",'\\1',gsub("___.*","",sui_names),perl = T),stringsAsFactors = F)
      index_df$SingleEntry<-index_df$names==index_df$rxsim1
      index_df$is_checkbox<-grepl("___",index_df$names)
      index_df$root_names<-index_df$names;index_df$checkbox_names<-NA
      index_df$root_names[index_df$is_checkbox]<-gsub("___.*$","",index_df$root_names[index_df$is_checkbox])
      index_df$checkbox_names[index_df$is_checkbox]<-gsub("___.*$","",index_df$root_names[index_df$is_checkbox])
      index_df$root_names<-gsub("_at[0-9]*$","\\1",index_df$root_names)
      return(index_df)
    }
      index_df<-bsrc.getSUIHX_index(protocol = ptcs$protect,suicide_formname = "ongoing_suicide_hx_lethality")
      sux_df<-bsrc.getform(protocol = ptcs$protect,grabnewinfo = F,formname = "ongoing_suicide_hx_lethality",batch_size=1000L)
      melt_sux_df<-reshape2::melt(sux_df,id.vars=c(index_df$names[index_df$SingleEntry]))
      meltxa<-cbind(melt_sux_df,index_df[match(as.character(melt_sux_df$variable),index_df$names),])
      meltxa<-meltxa[!meltxa$is_checkbox,]
      reshape_sux<-reshape2::dcast(meltxa,value.var = "value",formula = registration_redcapid+rxsim1~root_names, fun.aggregate = toString )
      reshape_sux->suihx
      suihx$rxsim1<-NULL
      suihx$sahx_describe<-NULL
      suihx %>% filter(!is.na(sahx_lr)| !is.na(sahx_sadate))->suihx
      ymd(suihx$sahx_sadate)->suihx$sahx_sadate
  #Root
      root="C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/"
  ###Access
    #Negative outcomes form
      #grab form
      negout<-read.csv(file = paste0(root, "A_NEGOUT.csv"))             
      negout[c(1,2,20:23)]->negout
      #fix dates
      as.Date(mdy(negout$CDATE))->negout$CDATE
      as.Date(mdy(negout$ATTDATE))->negout$ATTDATE
      as.Date(mdy(negout$ATTDATE2))->negout$ATTDATE2
      as.Date(mdy(negout$ATTDATE3))->negout$ATTDATE3
      as.Date(mdy(negout$ATTDATE4))->negout$ATTDATE4
      #Make one column with all atts and dates
      gather(negout,key="ATT",value="DATE", -ID,-CDATE)->negout2
      #date fix again  
      as.Date(negout2$DATE)->negout2$DATE
      #remove non-attempts
      negout2[-which(is.na(negout2$DATE)),]->negout2
      #Grab lethalities form
      leths<-read.csv(file = paste0(root, "S_LETH.csv"))
      #Remove 99's
      for (i in 1:nrow(leths[c(11,13, 15:21)])){
      leths[i,c(11,13, 15:21)]<-sapply(leths[i,c(11,13, 15:21) ], function(x){
      ifelse (x==99, x<-NA,x<-x)})}
      #Grab max leth (only one we care about)
      apply(leths[c(11,13, 15:21)], 1, function(x){max(x, na.rm=T)})->leths$max_leth
      #If infinity, all were NA
      leths[which(leths$max_leth==-Inf),"max_leth"]<-98
      #Only non-aborted attempts
      leths[which(leths$ABORT==1),"max_leth"]<-99
      #Fix CDATE
      as.Date(mdy(leths$CDATE))->leths$CDATE
      #remove unwanted vars
      leths[c(1,2,22)]->leths2
      #make new variable, id and cdate
      negout2 %>% mutate(newvar=paste0(ID, CDATE))->negout2
      leths %>% mutate(newvar=paste0(ID, CDATE))->leths
      #combine based on newvar
      leths$max_leth[match(negout2$newvar, leths$newvar)]->negout2$max_leth
      #Two people's CDATEs were off by 1 day, add in manually
      negout2[which(negout2$ID=="114567"),"max_leth"]<-4
      negout2[which(negout2$ID=="114572"),"max_leth"]<-8
      #Remove 99's, these are aborted attempts
      negout2[-which(negout2$max_leth==99),]->negout2
      #remove unwanted variables
      negout2[c(1,4,6)]->negout3
      names(negout3)[3]<-"Lethality"
    #Suicide questions form
      #Grab form
      suique<-read.csv(file = paste0(root, "S_SQUEST.csv"))
      suique[c(1,2,16,18,20,22,24,25,27,28,30,31,33,34,36,37)]->suique
      #fix dates
      as.Date(mdy(suique$CDATE))->suique$CDATE
      as.Date(mdy(suique$MRATTMPT))->suique$MRATTMPT
      as.Date(mdy(suique$MLDATE))->suique$MLDATE
      as.Date(mdy(suique$DATE1))->suique$DATE1
      as.Date(mdy(suique$DATE2))->suique$DATE2
      as.Date(mdy(suique$DATE3))->suique$DATE3
      as.Date(mdy(suique$DATE4))->suique$DATE4
      as.Date(mdy(suique$DATE5))->suique$DATE5
      #put dates with lethalities
        suique[c(1,3,4)]->suiatt1
        names(suiatt1)[c(2,3)]<-c("DATE","Lethality")
        suique[c(1,5:6)]->suiatt2
        names(suiatt2)[c(2,3)]<-c("DATE","Lethality")
        suique[c(1,7,9,11,13,15)]->suique
        gather(suique,key="ATT",value="DATE", -ID)->suique
        suique[-2]->suique
        na.rm(suique)->suique
        full_join(suique,suiatt1)->suiques
        full_join(suiques,suiatt2)->suiques
        suiques[which(!is.na(suiques$DATE)),]->suiques
      #Put together
        rbind(suiques, negout3)->atts
        atts->accATTs
        #full_join(negouts,suiques)->accATTs
        
    #Change IDs and match redcap and access
        accATTs<-bsrc.findid(accATTs,idmap = idmap,id.var = "ID")
        accATTs[2:4]->accATTs
        suihx<-bsrc.findid(suihx,idmap = idmap,id.var = "registration_redcapid")
        suihx<-data.frame(masterdemoid=suihx$masterdemoid, Lethality=suihx$sahx_lr,
                          DATE=suihx$sahx_sadate)
        as.integer(as.character(suihx$Lethality))->suihx$Lethality
        full_join(suihx,accATTs)->suicideatts
  #Remove duplicate attempts
    #make new variable with date and id
    suicideatts %>% mutate(newvar=paste0(masterdemoid, DATE))->suicideatts
    #order by lethality then check for duplicated 
    suicideatts[order(suicideatts$Lethality),]->suicideatts
    suicideatts[-which(duplicated(suicideatts$newvar) & is.na(suicideatts$Lethality)),]->suicideatts
    #Two people have duplicate (same date) attempts w/ different lethalities, taking date and highest lethality
    suicideatts[-which((suicideatts$newvar=="1146762003-03-12" | suicideatts$newvar=="2204072016-05-20") &
                        (suicideatts$Lethality==1 | suicideatts$Lethality==5)),]->suicideatts
    #Remove all other duplicates (same date AND lethality)
    suicideatts[-which(duplicated(suicideatts)),]->suicideatts
    suicideatts[-4]->atts_leths
  #Determine date of first attempt and highest lethality
    atts_leths[which(atts_leths$Lethality=="99"),"Lethality"]<-NA
    atts_leths %>% group_by(masterdemoid) %>% mutate(highestleth=max(na.omit(Lethality)))->atts_leths
    #People w/o Lethalities: Look into, remove for now
    atts_leths[which(atts_leths$highestleth==-Inf),"masterdemoid"]->noleths
    atts_leths[-which(atts_leths$masterdemoid %in% noleths$masterdemoid),]->atts_leths
    atts_leths %>% group_by(masterdemoid) %>% mutate(firstatt=min(na.omit(DATE)))->atts_leths
  #Determine ml att
    atts_leths[which(atts_leths$highestleth==atts_leths$Lethality),c("masterdemoid", "DATE")]->newdf
    newdf[-which(duplicated(newdf$masterdemoid)),]->newdf
    newdf$DATE[match(atts_leths$masterdemoid, newdf$masterdemoid)]->atts_leths$ml_date
    #Put first att and hl in Demo
    atts_leths$highestleth[match(MCQwdemo$masterdemoid, atts_leths$masterdemoid)]->MCQwdemo$highestlethatt
    atts_leths$ml_date[match(MCQwdemo$masterdemoid, atts_leths$masterdemoid)]->MCQwdemo$mldate
    atts_leths$firstatt[match(MCQwdemo$masterdemoid, atts_leths$masterdemoid)]->MCQwdemo$firstatt
  #Age of ml att
    age_calc(MCQwdemo[which(!is.na(MCQwdemo$mldate)),"DOB"], enddate=MCQwdemo[which(!is.na(MCQwdemo$mldate)),"mldate"],
             units="years", precise=F)->MCQwdemo[which(!is.na(MCQwdemo$mldate)),"agemlatt"]
  #Age of 1st att
    age_calc(MCQwdemo[which(!is.na(MCQwdemo$firstatt)),"DOB"], enddate=MCQwdemo[which(!is.na(MCQwdemo$firstatt)),"firstatt"],
             units="years", precise=F)->MCQwdemo[which(!is.na(MCQwdemo$firstat)),"agefirstatt"]
    
#SIS
  sis<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$bq_date,sis_score=p2$data$sis_max_total_s, sis_plan=p2$data$sis_max_planning_s)
  sis[which(!is.na(sis$sis_score)),]->sis
  as.character(sis$ID)->sis$ID
  as.Date(sis$date)->sis$date
  date.match.sis(x=sis,y=MCQwdemo, id=T, cutoff=190)->sis
  sis[c(2:5)]->sis
  #Access
  sisold<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_SINT.csv")
  sisold %>% filter(TIMEFRM!=9)->sisold
  sisold[-which(rowMeans(is.na(sisold))>0.5),]->sisold
  sisold %>% group_by(ID, CDATE) %>% filter(TIMEFRM==max(TIMEFRM))->sisold
  recode(sisold$Q8, "0"=2,"2"=0, "1"=1)->sisold$Q8
  as.character(sisold$ID)->sisold$ID
  rowSums(sisold[6:20])->sisold$score
  rowSums(sisold[6:13])->sisold$planningsub
  sisold$CDATE->sisold$date
  as.data.frame(sisold)->sisold
  date.match.sis(x=sisold, y=MCQwdemo, id=T, cutoff=190)->sisold
  sisold[c(49:52)]->sisold
  #Combine
  sis<-data.frame(masterdemoid=sis$masterdemoid, date=sis$date, planningsub=sis$sis_plan,
                  score=sis$sis_score,stringsAsFactors = F)
  rbind(sis,sisold)->SIS
  date.match.sis(x=SIS, id=F, cutoff=190)->SIS
  SIS$score[match(MCQwdemo$masterdemoid, SIS$masterdemoid)]->MCQwdemo$max_sis_total
  SIS$planningsub[match(MCQwdemo$masterdemoid, SIS$masterdemoid)]->MCQwdemo$max_plan_sub

#SCID data
  #Access
  SCID<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_SCIDIV.csv")
  mdy(SCID$CDATE)->SCID$date
  date.match.ssi(SCID, id=T, cutoff=365)->SCID
    #NAs mean 0 (for subs), change them
    for (i in 1:nrow(SCID)){
    SCID[i, c(108:126)]<- sapply(SCID[i, c(108:126)], function(x){ifelse(is.na(x) ,x<-0,x<-x)})}
    #calculate lifetime and pm subs use d/os
    sapply(1:nrow(SCID), function(x)sum(SCID[x,c(108,110,112, 114, 116, 118, 120, 122, 125)]>1))->SCID$LPsubs
    sapply(1:nrow(SCID), function(x)sum(SCID[x,c(109,111,113, 115, 117, 119, 121, 123, 126)]>1))->SCID$PMsubs
  #Redcap
    bsrc.getform(protocol=ptcs$protect, formname = "scid", online=F)->scid
    scid[c(1, 6, 114,116, 120, 122, 126, 128, 131, 133, 137, 139, 143, 145, 149, 151, 154, 156, 168, 170)]->scid
    #Same w/ NAs
    for (i in 1:nrow(scid)){scid[i,]<- sapply(scid[i,], function(x){ifelse(is.na(x) ,x<-0,x<-x)})}
    #calculate lifetime and pm subs
    sapply(1:nrow(scid), function(x)sum(scid[x,c(3,5,7,9,11,13,15,17,19)]>1))->scid$LPsubs
    sapply(1:nrow(scid), function(x)sum(scid[x,c(4,6,8,10,12,14,16,18,20)]>1))->scid$PMsubs
    #ID fix
    scid<-bsrc.findid(scid,idmap = idmap,id.var = "registration_redcapid")
    #Remove duplicates (take Access over redcap)
    scid[-which(scid$masterdemoid %in% SCID$masterdemoid),]->scid
  #Combine
    scid2<-data.frame(ID=scid$masterdemoid, LPsubs=scid$LPsubs, PMsubs=scid$PMsubs)
    SCID2<-data.frame(ID=SCID$masterdemoid, LPsubs=SCID$LPsubs, PMsubs=SCID$PMsubs)
    rbind(scid2,SCID2)->SCIDfin
  #Put in df
    SCIDfin$LPsubs[match(MCQwdemo$masterdemoid, SCIDfin$ID)]->MCQwdemo$lpsubs
    SCIDfin$PMsubs[match(MCQwdemo$masterdemoid, SCIDfin$ID)]->MCQwdemo$pmsubs
    
#ATHF (BL)
  #Access
    ATHF<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_ATHF.csv")
    mdy(ATHF$CDATE)->ATHF$date
    date.match.ssi(ATHF, id=T, cutoff=90)->ATHF
  #Redcap
    athf<-data.frame(ID=p2$data$registration_redcapid, event=p2$data$redcap_event_name, date=p2$data$bq_date,
                     score=p2$data$athf_maxnum)
    as.character(athf$ID)->athf$ID
    bsrc.findid(athf,idmap = idmap,id.var = "ID")->athf
    athf[which(grepl("baseline", athf$event)),]->athf
  #Combine
    athf2<-data.frame(masterdemoid=athf$masterdemoid, date=as.Date(athf$date), score=athf$score)
    ATHF2<-data.frame(masterdemoid=ATHF$masterdemoid, date=ATHF$date, score=ATHF$STRENGTH)
    rbind(athf2, ATHF2)->ATHFnew
    date.match.ssi(x=ATHFnew, id=F, cutoff=90)->ATHFnew
    ATHFnew$score[match(MCQwdemo$masterdemoid, ATHFnew$masterdemoid)]->MCQwdemo$ATHF_score
    
#CIRSG(total)
   #Access
    CIRS<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_CIRSG.csv")
    mdy(CIRS$CDATE)->CIRS$date
    date.match.ssi(CIRS, id=T, cutoff=90)->CIRS
    #score
    rowSums(CIRS[5:17])->CIRS$score
  #Redcap
    cirs<-data.frame(ID=p2$data$registration_redcapid, event=p2$data$redcap_event_name, date=p2$data$bq_date,
                     cirs1=p2$data$cirsg_1_s, cirs2=p2$data$cirsg_2_s, cirs3=p2$data$cirsg_3_s, 
                     cirs4=p2$data$cirsg_4_s, cirs5=p2$data$cirsg_5_s, cirs6=p2$data$cirsg_6_s,
                     cirs7=p2$data$cirsg_7_s, cirs8=p2$data$cirsg_8_s, cirs9=p2$data$cirsg_9_s,
                     cirs10=p2$data$cirsg_10_s, cirs11=p2$data$cirsg_11_s, cirs12=p2$data$cirsg_12_s,
                     cirs13=p2$data$cirsg_13_s)
    as.character(cirs$ID)->cirs$ID
    bsrc.findid(cirs,idmap = idmap,id.var = "ID")->cirs
    cirs[which(grepl("baseline", cirs$event)),]->cirs
    #score
    rowSums(cirs[4:16])->cirs$score
    cirs[-which(is.na(cirs$score)),]->cirs
  #Combine
    CIRS2<-data.frame(masterdemoid=CIRS$masterdemoid, date=CIRS$date, score=CIRS$score)
    cirs2<-data.frame(masterdemoid=cirs$masterdemoid, date=cirs$date, score=cirs$score)
    rbind(CIRS2, cirs2)->cirsnew
    date.match.ssi(cirsnew, id=F, cutoff=90)->cirsnew
    cirsnew$score[match(MCQwdemo$masterdemoid, cirsnew$masterdemoid)]->MCQwdemo$CIRS_score
      

#Make final df
    Finaldf<-data.frame(ID=MCQwdemo$masterdemoid, Item=MCQwdemo$Item, Response=MCQwdemo$Response, 
                        Immediate.magnitude=MCQwdemo$Immediate.magnitude, Delayed.magnitude=MCQwdemo$Delayed.magnitude,
                        Lenth.of.delay=MCQwdemo$Length.of.delay, Group=MCQwdemo$group, Age=MCQwdemo$bl.age, Gender=MCQwdemo$gender,
                        Race=MCQwdemo$race, Ethnicity=MCQwdemo$ethnicity, Education=MCQwdemo$edu, Marital.status=MCQwdemo$marital,
                        Income=MCQwdemo$income, ham17.score=MCQwdemo$ham17score, ham24.score=MCQwdemo$ham24score,
                        SSI.score=MCQwdemo$bl.SSIscore, SIS.score=MCQwdemo$max_sis_total, SIS.plansub=MCQwdemo$max_plan_sub,
                        mmse.score=MCQwdemo$mmse_score, drs.score=MCQwdemo$drs_score, wtar.score=MCQwdemo$wtar_score,
                        exit.score=MCQwdemo$exit_score, lifetime.subs=MCQwdemo$lpsubs, current.subs=MCQwdemo$pmsubs, 
                        athf.score=MCQwdemo$ATHF_score, cirs.score=MCQwdemo$CIRS_score,
                        highest_lethality=MCQwdemo$highestlethatt, age.firstatt=MCQwdemo$agefirstatt, 
                        age.mlatt=MCQwdemo$agemlatt)
  
  #CURRENTLY remove bad ids
    #badids
    #Finaldf[which(!Finaldf$ID %in% badids),]->Finaldf
  #Add variable for who is in Dombrovski 2011 paper
    dom2011<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/Dombrovski 2011 subs.csv")
    as.numeric(gsub(",","",as.character(dom2011$ID)))->dom2011$ID
    bsrc.findid(dom2011,idmap = idmap,id.var = "ID")->dom2011
    Finaldf %>% mutate(dom2011overlap=ifelse(Finaldf$ID %in% dom2011$masterdemoid,1,0))->Finaldf

    #Which people are in AFSP
    afspsubs<-read.csv(file="C:/Users/buerkem/OneDrive - UPMC/Desktop/Copy of AFSP PITT Correct Group Assignments.csv")
    as.numeric(as.character(afspsubs$subject))->afspsubs$subject
    bsrc.findid(afspsubs,idmap = idmap,id.var = "subject")->afspsubs
    length(unique(Finaldf[which(Finaldf$ID %in% afspsubs$masterdemoid),"ID"]))
    Finaldf %>% mutate(afspoverlap=ifelse(Finaldf$ID %in% afspsubs$masterdemoid, 1,0))->Finaldf
    
    
#Write data to file
  #Exclude 4 ATTs w/o leth data, 1 w/o ANY att data
    Finaldf[-which(Finaldf$Group=="ATT" & is.na(Finaldf$highest_lethality)),]->Finaldf
    write.csv(Finaldf,"C:/Users/buerkem/Box/skinner/data/delay discounting/MCQwithdemo3.csv")

#No IDEs with att
    Finaldf[which(Finaldf$Group=="IDE" & !is.na(Finaldf$highest_lethality)),]
#This is fine
    unique(Finaldf[which(Finaldf$highest_lethality==0),"ID"])

