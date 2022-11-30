
#This script takes the downloaded extended tornado alerts survey from Qualtrics (the survey with invitations/questionnaires) sent by mail
#Cleans it, calc weights and saves it for further analysis

#Packages you might need to install (if you don't have them)
#install.packages("haven")
#install.packages("survey")
#install.packages("srvyr")

remove(list = ls())
library(dplyr)
library(xtable)
library(haven)
library(tidyr)
library(sjlabelled)


require("haven")

#Put your own data folder here:
setwd("C:/Tornado_warnings/Benefits_ProbWarnings")

#IMPORT THE MAIN DATA:#
qsur<-readRDS(file="./Input/Ext_Alerts.Rdata")
dropped_vars=c('StardDate','DistributionChannel','EndDate','RecordedData', 'LS', 'EmbeddedData', 'term')
qsur<-qsur[, !(colnames(qsur) %in% dropped_vars)]

write.csv(qsur,"./Input/Data_Mail.csv", row.names = FALSE)

#Reading csv data with raw results:
qsur<-read.csv(file = './Input/Data_Mail.csv')

#Keep only complete responses##
print(table(qsur$Finished))
qsur<-subset(qsur,Finished==1)
qsur<-subset(qsur,gc==1)



#RENAMING
names(qsur)[names(qsur) == "F1"] <- "gender"
names(qsur)[names(qsur) == "F4"] <- "hisp_origin"
names(qsur)[names(qsur) == "F6"] <- "educ"
names(qsur)[names(qsur) == "B1"] <- "home"
names(qsur)[substr(names(qsur),1,5) == "Durat"] <- "Duration"

qsur$F2_1[qsur$F2_1==-99]<-NA
qsur$birthyear<-qsur$F2_1+1930-2
qsur$age<-2020-qsur$birthyear
qsur$age_group<-cut(qsur$age,c(0,34,59,Inf))
qsur$age_group<-recode(qsur$age_group,'(0,34]'=0,'(34,59]'=1,'(59,Inf]'=2)
qsur$age_groupf<-factor(qsur$age_group,levels=c(0,1,2),labels=c("<35","35-59","60+"),ordered=TRUE)
print(table(qsur$age_groupf))


qsur$Income<- recode(qsur$F7,'-99'=NA_real_, '1'=5000, '2'=17500, '3'=37500, '4'=62500, '5'=87500, '6'=112500, '7'=137500, '8'=175000,'9'=NULL)
qsur$logincome<-log(qsur$Income)
qsur$goodhouse<-recode(qsur$home,'-99'=NULL,'2'=1,'3'=1,.default=0)
table(qsur$Income)

table(qsur$F3)
qsur$hhsize=qsur$F3
qsur$hhsize[qsur$F3==-99]=NA
qsur$hhsize[qsur$F3==100]=NA
table(qsur$hhsize)

#Importing the pilot
pilot<-readRDS(file="./Temp/Ext_Alerts_Pilot.Rdata")
dim(qsur)
table(qsur$gender)
qsur<- bind_rows(qsur,pilot)

dim(qsur)
table(qsur$gender)

#Importing and merging sample characteristics (stratas)
sample_meta<-read.csv(file = './Input/sample_frame_characteristics.csv') 
head(sample_meta)
names(sample_meta)[names(sample_meta) == "stratas"] <- "strata" #selection probabilities by strata

sel_probs<-read.csv(file = './Input/selprobs.csv')
head(sel_probs)

sample_meta<-merge(sample_meta, sel_probs, by="strata",all.x = TRUE)


qsur<-merge(qsur,sample_meta,by=c("ExternalDataReference"),all.x = TRUE)
print(table(qsur$statefips))
print(table(qsur$strata))
print(table(qsur$Q_Language))
table(sample_meta$strata[sample_meta$pilot==1])


#Attaching state fips to merge by state
states_crosswalk<-read.csv(file = './Input/state_codes.csv')
qsur<-merge(qsur,states_crosswalk,by=c("statefips"),all.x = TRUE)
print(table(qsur$state_name))

#keep only the data with known weights:
qsur<-subset(qsur,is.na(selprob)==FALSE)


#LABELING
qsur$gender[qsur$gender==3]=NA
qsur$gender<-factor(qsur$gender,levels = c(1,2),labels = c("Female","Male"),ordered=TRUE)
#qsur$race<-factor(qsur$race,levels = c(-99,1,2,3,4,5,6),labels = c("Skip","Native American","Asian","Black","Pacific Islander","White","Other"),ordered=TRUE)
qsur$educ[qsur$educ==-99]=NA
labels_educ<-c("No school","Grades 1-12, no HS diploma","HS diploma","Some college","Associate or bachelor's degree","Advanced degree")
qsur$educ<-factor(qsur$educ,levels = c(1,2,3,4,5,6),labels = labels_educ,ordered=TRUE)

labels_home<-c("Skip","Apartment/Condo","Well-built house","Well-built townhouse","Mobile home","Manufactured home","Other")
qsur$home<-factor(qsur$home,levels = c(-99,1,2,3,4,5,6),labels = labels_home,ordered=TRUE)



strata_labels<-c("Not a tornado-prone state", "Hisp tract not in a tornado-prone state", "Tornado-prone state", "Hisp Florida", "Hisp Georgia", "Hisp Illinois", "Hisp Oklahoma", "Hisp Texas")

qsur$strata_f<-factor(qsur$strata,levels = c(0,1,2,3,4,5,6,7),labels = strata_labels,ordered=TRUE)

qsur$groupn_f<-factor(qsur$groupn, levels=c(0,1,2,3), labels=c("Not a tornado-prone state (general)", "Not in a tornado-prone state (hisp)", "Tornado-prone state (general)", "Tornado-prone state (hispanics)"))
table(qsur$strata_f)

qsur$tornadoprone<-recode(qsur$groupn, '0'=0,'1'=0,'2'=1,'3'=1)
qsur$tornadopronef<-factor(qsur$tornadoprone, levels=c(1,0), labels=c("Tornado-prone","Not tornado-prone"), ordered=TRUE)

#Loading prepared ACS sample proportions:
load(file = "./Input/ACS_props_all.RData")
load(file = "./Input/ACS_props_sep.RData")







#Labeling multiple responses question A1 on ways to receive tornado warnings
FUN<-function(v) recode(v,'-99'=0, '1'=1, .default = 0)
A1q<-names(qsur)[substr(names(qsur),1,2)=="A1"&names(qsur)!="A1_8_TEXT"&substr(names(qsur),1,5)!="A1_DO"]
print(A1q)
qsur[,A1q] <- data.frame(apply(qsur[A1q], 2, FUN))

Info_sources<-c("Weather radio","Local radio","TV","Internet-based service","Cell phone app/SMS","Friend/family/neighbors","I usually do not receive warnings","Other")
#qsur<-qsur[,c(1:23,25,24,26:ncol(qsur))]

qsur[,A1q]<-set_label(qsur[,A1q],Info_sources)
print(get_label(qsur[,A1q]))
Info_sources<-set_label(Info_sources,"How do you usually receive tornado warnings?")


#Protective response variables:
C1_l<-names(qsur)[substr(names(qsur),1,2)=="C1"&names(qsur)!="C1_7_TEXT"]
C2_l<-names(qsur)[substr(names(qsur),1,2)=="C2"&names(qsur)!="C2_7_TEXT"]
D1_l<-names(qsur)[substr(names(qsur),1,2)=="D1"&names(qsur)!="D1_6_TEXT"]
names(qsur)[substr(names(qsur),1,5)=="D2_10"]<-"D2_7"
D2_l<-names(qsur)[substr(names(qsur),1,2)=="D2"&names(qsur)!="D2_6_TEXT"]





FUN<-function(v) recode(v,'-99'=0, '1'=1, .missing=0, .default = 0)
qsur[,C1_l] <- data.frame(apply(qsur[C1_l], 2, FUN))
qsur[,C2_l] <- data.frame(apply(qsur[C2_l], 2, FUN))
qsur[,D1_l] <- data.frame(apply(qsur[D1_l], 2, FUN))
qsur[,D2_l] <- data.frame(apply(qsur[D2_l], 2, FUN))

#Convert multiple-response protective decision questions to multiple-choice questions (as overwhelming maj gives a single answer)
qsur %>% mutate(C1=C1_1+2*C1_2+3*C1_3+4*C1_4+5*C1_5+6*C1_6+7*C1_7) %>%
  mutate(C2=C2_1+2*C2_2+3*C2_3+4*C2_4+5*C2_5+6*C2_6+7*C2_7) %>%
  mutate(D1=D1_1+2*D1_2+3*D1_3+4*D1_4+5*D1_5+6*D1_6+7*D1_7) %>%
  mutate(D2=D2_1+2*D2_2+3*D2_3+4*D2_4+5*D2_5+6*D2_6+7*D2_7) %>%
  mutate(C1=recode(C1,`0`=NA_real_),C2=recode(C2,`0`=NA_real_),D1=recode(D1,`0`=NA_real_),D2=recode(D2,`0`=NA_real_))->qsur

print(table(qsur$C1))
print(table(qsur$C2))


#CLEANING THE DATA
qsur$nchoices_C1=rowSums(qsur[,C1_l])
print(table(qsur$nchoices_C1))
print(qsur[qsur$nchoices_C1==2,C1_l])
print(qsur$C1_7_TEXT[qsur$nchoices_C1==2])
print(qsur$AccessCode[qsur$nchoices_C1==2])
qsur$C1[qsur$AccessCode==106447]<-3


qsur$nchoices_C2=rowSums(qsur[,C2_l])
print(table(qsur$nchoices_C2))
print(qsur[qsur$nchoices_C2==2,C2_l])
print(qsur$C2_7_TEXT[qsur$nchoices_C2==2])
print(qsur$AccessCode[qsur$nchoices_C2==2])
qsur$C2[qsur$AccessCode==474456]<-2


qsur$nchoices_D1=rowSums(qsur[,D1_l])
print(table(qsur$nchoices_D1))
print(qsur[qsur$nchoices_D1==2,c("AccessCode",D1_l)])
qsur$D1[qsur$AccessCode==3412]<-6
qsur$D1[qsur$AccessCode==318591]<-6


qsur$nchoices_D2=rowSums(qsur[,D2_l])
print(table(qsur$nchoices_D2))
qsur$D2[qsur$AccessCode==307055]<-6

protect_responses<-c("Nothing","Seek more information without taking shelter","Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path","I would never learn about the warning at that time", "Other")
qsur$C1<-factor(qsur$C1,levels = c(1,2,3,4,5,6,7),labels = protect_responses,ordered=TRUE)
qsur$C2<-factor(qsur$C2,levels = c(1,2,3,4,5,6,7),labels = protect_responses,ordered=TRUE)
qsur$D1<-factor(qsur$D1,levels = c(1,2,3,4,5,6,7),labels = protect_responses,ordered=TRUE)
qsur$D2<-factor(qsur$D2,levels = c(1,2,3,4,5,6,7),labels = protect_responses,ordered=TRUE)
print(table(qsur$C1))
print(table(qsur$C2))

# probabilistic risk perception question:
C5_l<-names(qsur)[substr(names(qsur),1,3)=="C5-"&substr(names(qsur),8,11)!="TEXT"]



# switches in the probabilistic risk perception:
qsur$sw5<-0
qsur$secsw5<-0
count<-1

for(v in C5_l) {
  print(table(qsur[,v]))
  qsur$sw5[which((qsur[,v]>1)&(qsur[,v]<5)&(qsur$sw5==0))]<-count
  qsur$secsw5[which((qsur[,v]%in%c(1,5))&(qsur$sw5>0))]<-count
  print(v)
  count<-count+1
}

table(qsur$sw5)
table(qsur$secsw5)



#------weights----

#calculate proportions in the sample  and weights accounting for location:
qsur %>% select(gender, age_groupf, tornadopronef) %>%
  filter(!is.na(gender)&!is.na(age_groupf)) %>%
  mutate(total=n()) %>%
  group_by(gender, age_groupf, tornadopronef) %>% 
  summarise(num=n(),total=first(total)) %>%
  mutate(prop_sample=num/total) %>%
  select(gender, age_groupf, tornadopronef, prop_sample, num) %>%
  arrange(tornadopronef, age_groupf, gender)->M_props_sep
print(M_props_sep)
names(ACS_props_sep)=c("gender","age_groupf","tornadopronef","total_ACS", "prop_ACS")
M_props_sep<-merge(M_props_sep,ACS_props_sep,by=c("gender","age_groupf","tornadopronef"),all.x=TRUE)
M_props_sep$weight_s=M_props_sep$total_ACS/M_props_sep$num
print(M_props_sep)
M_props_sep %>% select(gender, age_groupf, tornadopronef, weight_s)->M_props_sep


#proportions in the sample and weights without accounting for location (more robust weights)
qsur %>% select(gender, age_groupf) %>%
  filter(!is.na(gender)&!is.na(age_groupf)) %>%
  mutate(total=n()) %>%
  group_by(gender, age_groupf) %>% 
  summarise(num=n(),total=first(total)) %>%
  mutate(prop_sample=num/total) %>%
  select(gender, age_groupf, prop_sample, num) %>%
  arrange(age_groupf, gender)->M_props_all
print(M_props_all)


names(ACS_props_all)=c("gender","age_groupf","total_ACS", "prop_ACS")
M_props_all<-merge(M_props_all,ACS_props_all,by=c("gender","age_groupf"),all.x=TRUE)
M_props_all$weight_a=M_props_all$prop_ACS/M_props_all$prop_sample
print(M_props_all)
M_props_all %>% select(gender, age_groupf, weight_a)->M_props_all

qsur<-merge(qsur,M_props_sep,by=c("gender","age_groupf","tornadopronef"),all.x=TRUE)
qsur<-merge(qsur,M_props_all,by=c("gender","age_groupf"),all.x=TRUE)

#weight_a - not separating by location, weight_s - separately for tornado-prone and other states
#Missing weights become one
qsur$weight_a[is.na(qsur$weight_a)]<-1

#---Importing tornado frequency by county---------
qsur$fips=1000*qsur$statefips+qsur$countyfips


qsur$AccessCode<-qsur$ExternalDataReference

qsur %>% filter(!is.na(ExternalDataReference)) ->qsur

saveRDS(qsur,file="./Temp/Ext_Alerts_Merged.Rdata")


