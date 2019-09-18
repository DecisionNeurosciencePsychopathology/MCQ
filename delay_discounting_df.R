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
     
#MCQdata  
  #get old MCQ data file
  MCQold<-read.csv(file ="C:/Users/buerkem/OneDrive - UPMC/Desktop/MCQ data/Copy of subjvalues_k.csv")
  #Change IDs on old MCQ data file
  MCQold<-bsrc.findid(MCQold,idmap = idmap,id.var = "ï..ID")
  MCQold[-c(1,9:12)]->MCQold
  #get new MCQ data file
  MCQnew<-p2$data[c("registration_redcapid","redcap_event_name","bq_date","mcq_1","mcq_2","mcq_3", "mcq_4", "mcq_5", "mcq_6",
        "mcq_7","mcq_8","mcq_9","mcq_10","mcq_11","mcq_12","mcq_13","mcq_14",
        "mcq_15","mcq_16","mcq_17","mcq_18","mcq_19","mcq_20","mcq_21",
        "mcq_22","mcq_23","mcq_24","mcq_25","mcq_26","mcq_27","mcq_28","mcq_29","mcq_30")]
  MCQnew %>% filter(redcap_event_name=="baseline_arm_2")->MCQnew
  #Figure out MCQnew ids
  MCQnew<-bsrc.findid(MCQnew,idmap = idmap,id.var = "registration_redcapid")
  MCQnew[-c(1, 2, 35:38)]->MCQnew
  #Only grab new MCQ data from new dataset
  MCQnew[which(!MCQnew$masterdemoid %in% MCQold$ï..ID),]->MCQnewunique
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
  #Fix old data
  mdy(MCQold$CDATE)->MCQold$CDATE
  #MCQold %>% group_by(masterdemoid) %>% filter(CDATE==min(CDATE)) %>%ungroup()->MCQold
  MCQgather$` masterdemoid`->MCQgather$masterdemoid
  as.Date(MCQgather$` bq_date`)->MCQgather$CDATE
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
  #Only ppl in finalMCQ
  demo[which(demo$masterdemoid %in% finalMCQ$masterdemoid),]->demo
  #fix race (X6=multiple races)
  demo %>% gather(key="race", value = "value", -c(masterdemoid, P2condate, P1condate, 
           S1condate, S2condate, DOB, group, gender, edu, ethnicity, marital))->demo
  demo %>% filter(value==1)->demo
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
  MCQwdemo[-which(is.na(MCQwdemo$consentdate)),]->MCQwdemo
  
  #Age at consent date (Must be 50+)
  as.Date(MCQwdemo$consentdate)->MCQwdemo$consentdate
  as.Date(MCQwdemo$DOB)->MCQwdemo$DOB
  MCQwdemo %>% mutate(bl.age=age_calc(DOB,enddate = consentdate, units="years", precise=F))->MCQwdemo
  MCQwdemo[which(MCQwdemo$bl.age>49),]->MCQwdemo
    #Number of pts: sum(table(unique(MCQwdemo$masterdemoid)))
  
  ###############LEFT OFF HERE##################
  
  #Household income (from Macarthur SDQ)
  #Redcap
  income<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$bq_date,
                     Event=p2$data$redcap_event_name, Income=p2$data$macarthur_6)
  income[which(income$Event=="baseline_arm_2"),]->income
  income[which(!is.na(income$Income)),]->income
  as.character(income$ID)->income$ID
  income<-bsrc.findid(income,idmap = idmap,id.var = "ID")
  income[-c(1,3,6:9)]->income
  #Access
  
  
  
  
  #Grab MMSE from redcap
  mmse<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$mmse_date, score=p2$data$mmse_s_adj)
  mmse[which()]
  
#Grab forms
  #grab DRS from redcap
  drs<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$drs_date, score=p2$data$drs_total)
  
  
  #grab WTAR from redcap
  wtar<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$wtar_date, score=p2$data$wtar_s_adj)
 
  
  
  #EXIT
    #grab EXIT from redcap and change ID
    exit<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$exit_date, score=p2$data$exit_total)
    as.character(exit$ID)->exit$ID
    exit<-bsrc.findid(exit,idmap = idmap,id.var = "ID")
    exit[which(!is.na(exit$score)),]->exit
    as.Date(exit$date)->exit$date
    exit[-c(1,5:8)]->exit
    #grab EXIT from Access
    exit2<-read.csv(file = "C:/Users/buerkem/OneDrive - UPMC/Documents/Data pulls/MCQ/A_EXIT_S.csv")
    exit2[-c(3:15)]->exit2
    #any scores over 97 means NA was involved, remove from total
    exit2[which(exit2$EXITtot>97),"EXITtot"]<-NA
    exit2[which(!is.na(exit2$EXITtot)),]->exit2
    #Date var
    exit2$CDate->exit2$date
    #Change IDS and date match
    date.match(exit2, id=T, cutoff=600)->exit2
    #fix dates after date match
    as.character(exit2$date)->exit2$date
    as.character(exit$date)->exit$date
    #Make new df
    exitold<-data.frame(masterdemoid=exit2$masterdemoid, date=exit2$date, score=exit2$EXITtot,stringsAsFactors = F)
    #rbind instead of merge (can't do this with duplicate data!!!!!!!!!)
    rbind(exit,exitold)->EXIT
    as.Date(EXIT$date) -> EXIT$date
    date.match(x=EXIT, id=F, cutoff = 600)->EXIT
    #merge into demo
    EXIT$score->EXIT$exit_total
    EXIT[-c(1,2,4,5)]->EXIT
    merge(EXIT, MCQwdemo,all=T)->MCQwdemo
    
    
  
    
  #Grab HAM from redcap
  ham<-data.frame(ID=p2$data$registration_redcapid,date=p2$data$ham_date, ham1=p2$data$ham_1_dm,
                  ham2=p2$data$ham_2_gf, ham3=p2$data$ham_3_su, ham4=p2$data$ham_4_ii, ham5=p2$data$ham_5_im,
                  ham6=p2$data$ham_6_di, ham7=p2$data$ham_7_wi, ham8=p2$data$ham_8_re,
                  ham9=p2$data$ham_9_ag, ham10=p2$data$ham_10_psya, ham11=p2$data$ham_11_soma,
                  ham12=p2$data$ham_12_gi, ham13=p2$data$ham_13_gs, ham14=p2$data$ham_14_sex,
                  ham15=p2$data$ham_15_hd,ham16=p2$data$ham_16_li, ham17=p2$data$ham_17_weight,
                  ham18=p2$data$ham_18_rt, ham19=p2$data$ham_19_dp,ham20=p2$data$ham_20_prsx,
                  ham21=p2$data$ham_21_ocsx,ham22=p2$data$ham_22_xhelp, ham23=p2$data$ham_23_xhope,
                  ham24=p2$data$ham_24_xworth)
  
  
  
  #Grab suicide hx from redcap (Jiazhou's help)
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
  
  
 
  
  
  
  #Write data to file
#library(xlsx)           
#write.xlsx(finalMCQ,"C:/Users/buerkem/Box/skinner/finalMCQ.xlsx")
           