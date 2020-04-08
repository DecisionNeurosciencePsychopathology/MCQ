#Meds addition
#setup
startup()
md<-bsrc.checkdatabase2(ptcs$masterdemo,forceskip = T,online = T,batch_size=1000L)
idmap<-md$data[c("registration_redcapid","registration_wpicid","registration_soloffid")]
names(idmap)<-c("masterdemoid","wpicid","soloffid")
p2<-bsrc.checkdatabase2(protocol=ptcs$protect, online=T, batch_size=1000L)
#library
library(lubridate)
library(reshape2)
library(tidyverse)
#Get data
read.csv("/Users/mogoverde/Box/skinner/projects_analyses/delay discounting data/MCQdata-2-5-2020.csv")->MCQdata
#Get date of MCQ
  #old data
  old<-read.csv(file ="/Users/mogoverde/Box/codes/MCQ data/Copy of subjvalues_k.csv")
  bsrc.findid(old, idmap=idmap, "ID")->old
  old$CDATE[match(MCQdata$masterdemoid, old$masterdemoid)]->MCQdata$CDATE
  mdy(MCQdata$CDATE)->MCQdata$CDATE
  #new data
  data.frame(ID=p2$data$registration_redcapid, CDATE=p2$data$bq_date, intials=p2$data$startup_init)->new
  as.character(new$ID)->new$ID
  bsrc.findid(new, idmap=idmap, "ID")->new
  new[which(new$CDATE!=""),]->new
  #MCQdata[which(is.na(MCQdata$CDATE)),c("masterdemoid")]->newids
  for(i in 1:nrow(MCQdata)){
    if(is.na(MCQdata[i,"CDATE"])){
      MCQdata[i,"masterdemoid"]->id
      new[which(new$masterdemoid==id),"CDATE"]->MCQdata[i,"CDATE"]}}
  as.Date(MCQdata$CDATE)->MCQdata$CDATE
#Grab meds
  meds<-data.frame(masterdemoid=md$data$registration_redcapid,
             md$data[grepl("medlist_startdate|medlist_enddate|medlist_name|medlist_spname",names(md$data))])
  meds[which(meds$masterdemoid %in% MCQdata$masterdemoid),]->meds
  reshape(meds, direction='long', 
        varying=names(meds)[-1], 
        timevar='time',
        times=1:150,
        v.names=c("spname","name","start","end"),
        idvar='masterdemoid')->meds3
  names(meds3)[3:6]<-c("end","spname","name","start")
  MCQdata$CDATE[match(meds3$masterdemoid,MCQdata$masterdemoid)]->meds3$CDATE
  as.Date(meds3$end)->meds3$end
  as.Date(meds3$start)->meds3$start
  meds3[which((!is.na(meds3$name) & meds3$name!="") | 
    (!is.na(meds3$spname) & meds3$spname!="")),]->meds4
  #Check these people they don't have start dates
  #meds4[which(is.na(meds4$start)),]->check
  #data.frame(ID=md$data$registration_redcapid,initials=md$data$registration_initials)->initials
  #initials$initials[match(check$masterdemoid, initials$ID)]->check$initials
  meds4[which(is.na(meds4$end)),]->check2
  meds4[which(is.na(meds4$end) | meds4$end>=meds4$CDATE),]->meds5
  meds5[which(meds5$start<=meds5$CDATE),]->meds5
  
skinner_dir = "/Users/mogoverde/Box/skinner/"
medlist_map<-read.csv(paste0(skinner_dir,"Data/medlist/Medlist Map_2020-04-08.csv"),stringsAsFactors = F)
result<-dplyr::left_join(meds5,medlist_map,by=c("spname"="original_value"))
# Drug: Cleaned drug name. Code: RxNorm code in the original value. RxDrugName: the drug that matchs RxNorm Code. 
#check if all rows mapped 

result %>% as_tibble() %>% filter(is.na(Drug)) # this give any row that is matched 
#below returns any row that has a non-NA drug but cannot find a match. good if returns 0 rows. 
result %>% mutate(spname = na_if(spname,"")) %>% filter(!is.na(spname)&is.na(Drug)) 
#Get number of each kind of med
result %>% group_by(masterdemoid) %>% mutate(sednum=sum(Sedative.hypnotic,na.rm=T))->result
result %>% group_by(masterdemoid) %>% mutate(opinum=sum(Opioid,na.rm=T))->result
result %>% group_by(masterdemoid) %>% mutate(antinum=sum(Anticholinergic,na.rm=T))->result
result %>% group_by(masterdemoid) %>% filter(row_number()==1)->result2
mutate(result2, seds=ifelse(sednum>0,1,0))->result2
mutate(result2, opis=ifelse(opinum>0,1,0))->result2
mutate(result2, antis=ifelse(antinum>0,1,0))->result2

result2[c("masterdemoid","sednum","opinum","antinum","seds","opis","antis")]->MEDS
write.csv(MEDS, "MCQ_MEDS_4-8-2020.csv")
  
  #setwd("/Users/mogoverde/Desktop")
  #write.csv(check,"meds_nostart.csv")
