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
     md<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T,online = T,batch_size=1000L)
     idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
     names(idmap)<-c("masterdemoid","wpicid","soloffid")
    #p2 map
     p2<-bsrc.checkdatabase2(protocol=ptcs$protect, online=T, batch_size=1000L)
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
  
#Suicide hx
  #Redcap
      bsrc.getSUIHX_index<-function(protocol=protocol.cur,suicide_formname="suicide_history"){
      metadata<-bsrc.getform(protocol = protocol,formname = suicide_formname,aggressivecog = F,mod = F,grabnewinfo = T,batch_size=1000L)
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
  sux_df<-bsrc.getform(protocol = ptcs$protect,grabnewinfo = T,formname = "ongoing_suicide_hx_lethality",batch_size=1000L)
  melt_sux_df<-melt(sux_df,id.vars=c(index_df$names[index_df$SingleEntry]))
  meltxa<-cbind(melt_sux_df,index_df[match(as.character(melt_sux_df$variable),index_df$names),])
  meltxa<-meltxa[!meltxa$is_checkbox,]
  reshape_sux<-reshape2::dcast(meltxa,value.var = "value",formula = registration_redcapid+rxsim1~root_names)
  reshape_sux->suihx
  suihx$rxsim1<-NULL
  suihx$sahx_describe<-NULL
  suihx %>% filter(!is.na(sahx_lr)| !is.na(sahx_sadate))->suihx
  ymd(suihx$sahx_sadate)->suihx$sahx_sadate
  
  #Date of first attempt and highest lethality
  suihx %>% group_by(registration_redcapid) %>% mutate(firstattrc=min(na.omit(sahx_sadate)))->suihx
  suihx %>% group_by(registration_redcapid) %>% mutate(maxlethrc=max(na.omit(sahx_lr)))->suihx
  suihx[which(suihx$sahx_lr==suihx$sahx_lr),c("registration_redcapid","sahx_sadate")]->mldates
  mldates[!duplicated(mldates$registration_redcapid),]->mldates
  as.data.frame(mldates)->mldates
  mldates$sahx_sadate[match(suihx$registration_redcapid, mldates$registration_redcapid)]->suihx$mldate
  suihx[c(1,4,5,6)]->suihx
  
  #Access suicide hx at followups
  negout<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_NEGOUT.csv")
  negout[c(1,9,10,12,13,15, 20:23)]->negout
    #fix dates
    mdy(negout$DATE1)->negout$DATE1
    mdy(negout$DATE2)->negout$DATE2
    mdy(negout$DATE3)->negout$DATE3
    mdy(negout$ATTDATE)->negout$ATTDATE
    mdy(negout$ATTDATE2)->negout$ATTDATE2
    mdy(negout$ATTDATE3)->negout$ATTDATE3
    mdy(negout$ATTDATE4)->negout$ATTDATE4
    #gather dates
    gather(negout, key="Att", value = "Date",-ID, -SUI1, -SUI2)->negout
    negout[-4]->negout
    negout %>% group_by(ID) %>% mutate(firstattfu=min(na.omit(Date)))->negout
    #gather lethalities  
    gather(negout, key = "Att", value="Lethality",-ID, -Date,-firstattfu)->negout
    negout[which(negout$Lethality>9),"Lethality"]<-NA
    negout %>% group_by(ID) %>% mutate(maxlethfu=max(na.omit(Lethality)))->negout
    negout[which(negout$maxlethfu<0),"maxlethfu"]<-NA
    negout[which(negout$maxlethfu==negout$Lethality),c("ID","Date")]->oldmldates
    oldmldates[!duplicated(oldmldates$ID),]->oldmldates
    oldmldates$Date[match(negout$ID, oldmldates$ID)]->negout$oldmldate
    negout[-c(2,4,5)]->negout
  #Suicide hx at baseline
  suique<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_SQUEST.csv")
  suique[c(1,14,16,18,20,22,23,25,26,28,29,31,32,34,35)]->suique
    #fix dates
    mdy(suique$MRATTMPT)->suique$MRATTMPT
    mdy(suique$MLDATE)->suique$MLDATE
    mdy(suique$DATE1)->suique$DATE1
    mdy(suique$DATE2)->suique$DATE2
    mdy(suique$DATE3)->suique$DATE3
    mdy(suique$DATE4)->suique$DATE4
    mdy(suique$DATE5)->suique$DATE5
    #gather dates
    gather(suique, key="Att", value="Date",-ID, -LETHMR,-LETHML,-LETH1,-LETH2,-LETH3,-LETH4,-LETH5)->suique
    suique[-9]->suique
    #gather lethality
    gather(suique, key="Att", value="Lethality", -ID, -Date)->suique
    suique[-3]->suique
    suique[which(suique$Lethality>9),"Lethality"]<-NA
    #First att and max leth
    suique %>% group_by(ID) %>% mutate(maxlethbl=max(na.omit(Lethality)))->suique
    suique[which(suique$maxlethbl<0),"maxlethbl"]<-NA
    suique %>% group_by(ID)%>% mutate(firstattbl=min(na.omit(Date)))->suique
    suique[which(suique$maxlethbl==suique$Lethality),c("ID","Date")]->oldmldates2
    oldmldates2[!duplicated(oldmldates2$ID),]->oldmldates2
    oldmldates2$Date[match(suique$ID, oldmldates2$ID)]->suique$oldmldate
    suique[-c(2,3)]->suique
  #Combine bl and fu in Access
    rbind(negout,suique)->ACsuihx
    gather(ACsuihx, key="key", value="lethality",-ID, -firstattfu,-firstattbl,-oldmldate)->ACsuihx
    gather(ACsuihx,key="key2", value="date",-ID, -key,-lethality, -oldmldate)->ACsuihx
    ACsuihx %>% group_by(ID) %>% mutate(ACmaxleth=max(na.omit(lethality)))->ACsuihx
    ACsuihx[which(ACsuihx$ACmaxleth<0),"ACmaxleth"]<-NA
    ACsuihx %>% group_by(ID) %>% mutate(ACfirstatt=min(na.omit(date)))->ACsuihx
    ACsuihx[which(ACsuihx$ACmaxleth==ACsuihx$lethality),c("ID","oldmldate")]->oldml
    oldml[!duplicated(oldml$ID),]->oldml
    oldml$oldmldate[match(ACsuihx$ID, oldml$ID)]->ACsuihx$oldml
    ACsuihx[c(1,7:9)]->ACsuihx
   #Fix IDs on both forms
    suihx->suihxrc
    as.data.frame(suihxrc)->suihxrc
    bsrc.findid(suihxrc,idmap = idmap,id.var = "registration_redcapid")->suihxrc
    suihxrc[c(2:5)]->suihxrc
    ACsuihx %>% group_by(ID) %>% filter(row_number()==1)->ACsuihx
    as.data.frame(ACsuihx)->ACsuihx
    as.data.frame(bsrc.findid(ACsuihx,idmap = idmap,id.var = "ID"))->ACsuihx
    ACsuihx[which(ACsuihx$ifexist==T),]->ACsuihx
    ACsuihx[c(2:5)]->ACsuihx
    row.names(ACsuihx)<-NULL
  #Merge and get vars
    suihxrc %>% group_by(masterdemoid) %>% filter(row_number()==1)->suihxrc
    data.frame(masterdemoid=suihxrc$masterdemoid,firstatt=suihxrc$firstattrc, maxleth=suihxrc$maxlethrc, 
               mldate=suihxrc$mldate)->suihxrc
    data.frame(masterdemoid=ACsuihx$masterdemoid,firstatt=ACsuihx$ACfirstatt, maxleth=ACsuihx$ACmaxleth,
               mldate=ACsuihx$oldml)->ACsuihx
    rbind(ACsuihx,suihxrc)->lethandfirstatt
    as.numeric(lethandfirstatt$maxleth)->lethandfirstatt$maxleth
    ymd(lethandfirstatt$firstatt)->lethandfirstatt$firstatt
    lethandfirstatt[which(!is.na(lethandfirstatt$firstatt) | !is.na(lethandfirstatt$maxleth)),]->lethandfirstatt
    lethandfirstatt %>% group_by(masterdemoid) %>% mutate(fin1statt=min(firstatt))->lethandfirstatt
    lethandfirstatt %>% group_by(masterdemoid) %>% mutate(finmaxleth=max(maxleth))->lethandfirstatt
    lethandfirstatt[which(lethandfirstatt$maxleth==lethandfirstatt$maxleth),c("masterdemoid","mldate")]->finml
    finml[!duplicated(finml$masterdemoid),]->finml
    finml$mldate[match(lethandfirstatt$masterdemoid, finml$masterdemoid)]->lethandfirstatt$finmldate
    #combine with demo df
    MCQwdemo[which(MCQwdemo$group=="ATT"),"masterdemoid"]->ATTs
    lethandfirstatt[which(lethandfirstatt$masterdemoid %in% ATTs),]->lethandfirstatt
    lethandfirstatt$finmaxleth[match(MCQwdemo$masterdemoid, lethandfirstatt$masterdemoid)]->MCQwdemo$mlatt
    lethandfirstatt$fin1statt[match(MCQwdemo$masterdemoid, lethandfirstatt$masterdemoid)]->MCQwdemo$firstatt
    lethandfirstatt$mldate[match(MCQwdemo$masterdemoid, lethandfirstatt$masterdemoid)]->MCQwdemo$mldate
    #####################################################
    #Missing attempt lethality
    unique(MCQwdemo[which(MCQwdemo$group=="ATT" & (is.na(MCQwdemo$mlatt) | is.na(MCQwdemo$firstatt))),"masterdemoid"])->badids
    
    #Find age at first attempt
    MCQwdemo$age_1statt=ifelse(!is.na(MCQwdemo$firstatt),age_calc(na.omit(MCQwdemo$firstatt), units="years", precise=F), NA)
    
    ###################################################
    
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
    
#Make final df
    Finaldf<-data.frame(ID=MCQwdemo$masterdemoid, Item=MCQwdemo$Item, Response=MCQwdemo$Response, 
                        Immediate.magnitude=MCQwdemo$Immediate.magnitude, Delayed.magnitude=MCQwdemo$Delayed.magnitude,
                        Lenth.of.delay=MCQwdemo$Length.of.delay, Group=MCQwdemo$group, Age=MCQwdemo$bl.age, Gender=MCQwdemo$gender,
                        Race=MCQwdemo$race, Ethnicity=MCQwdemo$ethnicity, Education=MCQwdemo$edu, Marital.status=MCQwdemo$marital,
                        Income=MCQwdemo$income, ham17.score=MCQwdemo$ham17score, ham24.score=MCQwdemo$ham24score,
                        SSI.score=MCQwdemo$bl.SSIscore, SIS.score=MCQwdemo$max_sis_total, SIS.plansub=MCQwdemo$max_plan_sub,
                        mmse.score=MCQwdemo$mmse_score, drs.score=MCQwdemo$drs_score, wtar.score=MCQwdemo$wtar_score,
                        exit.score=MCQwdemo$exit_score, highest_lethality=MCQwdemo$mlatt, age.firstatt=MCQwdemo$age_1statt)
  
  #CURRENTLY remove bad ids
    #badids
    Finaldf[which(!Finaldf$ID %in% badids),]->Finaldf
  #Add variable for who is in Dombrovski 2011 paper
    dom2011<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/Dombrovski 2011 subs.csv")
    as.numeric(gsub(",","",as.character(dom2011$ID)))->dom2011$ID
    bsrc.findid(dom2011,idmap = idmap,id.var = "ID")->dom2011
    Finaldf %>% mutate(overlap=ifelse(Finaldf$ID %in% dom2011$masterdemoid,1,0))->Finaldf

    #Which people are in AFSP
    afspsubs<-read.csv(file="C:/Users/buerkem/OneDrive - UPMC/Desktop/Copy of AFSP PITT Correct Group Assignments.csv")
    as.numeric(as.character(afspsubs$subject))->afspsubs$subject
    bsrc.findid(afspsubs,idmap = idmap,id.var = "subject")->afspsubs
    length(unique(Finaldf[which(Finaldf$ID %in% afspsubs$masterdemoid),"ID"]))
    
    
#Write data to file
write.csv(Finaldf,"C:/Users/buerkem/Box/skinner/data/delay discounting/MCQwithdemo.csv")
           