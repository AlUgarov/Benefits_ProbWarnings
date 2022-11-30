
#Importing and preparing for analysis the Internet-sample

remove(list = ls())
library(dplyr)
library(xtable)
library(ggplot2)
library(haven)
library(tidyr)
library(sjlabelled)
library(qualtRics)
library(httr)
library(survey)

tempdir()
dir.create(tempdir())


unloadNamespace("memisc")

#Put your own data folder here:
require("haven")


#Reading csv data with raw results:
qsur<-read.csv(file = '../Input/Tornado_Warnings_Qualtrics.csv')



#Keep only complete responses##
qsur<-subset(qsur,Finished==1)
qsur<-subset(qsur,gc==1)
table(qsur$group)


#Select and flag pilot samples:
qsur$date<-qsur$StartDate
qsur$pilot<-0
qsur$pilot[as.Date(qsur$date)<as.Date('2020-07-05')]<-1
qsur$pilot[as.Date(qsur$date)>as.Date('2020-07-05')&as.Date(qsur$date)<as.Date('2020-07-12')]<-2
qsur$pilot[as.Date(qsur$date)>as.Date('2020-07-14')&as.Date(qsur$date)<as.Date('2020-07-16')]<-3

table(qsur$pilot)
#qsur<-subset(qsur,pilot==0)

#RENAMING
names(qsur)[names(qsur) == "F1"] <- "gender"
#names(qsur)[names(qsur) == "F6"] <- "race"
names(qsur)[names(qsur) == "F7"] <- "educ"
names(qsur)[names(qsur) == "B1"] <- "home"
names(qsur)[substr(names(qsur),1,5) == "Durat"] <- "Duration"

qsur$birthyear<-qsur$F2_1+1930-2
qsur$age<-2020-qsur$birthyear
qsur$age_group<-cut(qsur$age,c(0,34,59,Inf))
qsur$age_group<-recode(qsur$age_group,'(0,34]'=0,'(34,59]'=1,'(59,Inf]'=2)

qsur$hhsize<-qsur$F3
qsur$hhsize[qsur$hhsize<1]<-NA

print(table(qsur$age_group))



#Attaching state fips to merge by state
names(qsur)[names(qsur) == "State"] <- "state_q"
states_crosswalk<-read.csv(file = '../Input/state_codes.csv')
qsur<-merge(qsur,states_crosswalk,by=c("state_q"),all.x = TRUE)


#Attaching weights
df_weights<-read.csv(file = '../Input/sample_weights1.csv') #weights by gender and group, if gender+group+age+state - use sample_weights.csv
head(df_weights)
qsur<-merge(qsur,df_weights,by=c("gender","group"),all.x = TRUE)
#qsur<-merge(qsur,df_weights,by=c("gender","group","age_group","statefips"),all.x = TRUE)
#Collapse the survey data:

print(summary(qsur$perwt0))

qsur$xid<-1
qtotal<-aggregate(xid~gender+group,data=qsur,sum)
qsur<-merge(qsur,qtotal,by=c("gender","group"),all.x = TRUE)
names(qsur)[names(qsur) == "xid.y"] <- "qtotal"
names(qsur)[names(qsur) == "xid.x"] <- "xid"
qsur$qtotal<-replace_na(qsur$qtotal,0)

qtotal_g<-aggregate(xid~group,data=qsur,sum) #aggregate Qualtrics within each group
qsur<-merge(qsur,qtotal_g,by=c("group"),all.x = TRUE)
names(qsur)[names(qsur) == "xid.y"] <- "qtotal_g"
names(qsur)[names(qsur) == "xid.x"] <- "xid"

qsur$pweight<-10000*qsur$perwt0/qsur$qtotal
qsur$pweight_g<-10000*qsur$perwt_g/(qsur$qtotal)

print(summary(qsur$pweight_g))
print(summary(qsur$pweight))



### CALCULATE AUXILIARY VARIABLES ###

protectC1<-as.numeric((qsur$C1>1)&(qsur$C1<6))
protectD1<-as.numeric((qsur$D1>1)&(qsur$D1<6))
qsur$Income<- recode(qsur$F8,'1'=5000, '2'=17500, '3'=37500, '4'=62500, '5'=87500, '6'=112500, '7'=137500, '8'=175000,'9'=NULL)
qsur$logincome<-log(qsur$Income)
qsur$goodhouse<-recode(qsur$home,'-99'=NULL,'2'=1,'3'=1,.default=0)
#Protection decision vs no action, seek info or other
protectC1s<-as.numeric((qsur$C1>2)&(qsur$C1<6))
protectD1s<-as.numeric((qsur$D1>2)&(qsur$D1<6))

#Lists of variable names handles (used both in labeling and cleaning)
C7_l<-names(qsur)[substr(names(qsur),1,2)=="C7"]
C8_l<-names(qsur)[substr(names(qsur),1,2)=="C8"]

D2_l_old<-c("D2_1","D2_2","D2_3","D2_10","D2_11","D2_12")
D4_l_old<-c("D4_1","D4_2","D4_3","D4_10","D4_11","D4_12")

D2_l<-c("D2_1","D2_2","D2_3","D2_4","D2_5","D2_6")
D4_l<-c("D4_1","D4_2","D4_3","D4_4","D4_5","D4_6")

qsur$D2_4<-qsur$D2_10
qsur$D2_5<-qsur$D2_11
qsur$D2_6<-qsur$D2_12

qsur$D4_4<-qsur$D4_10
qsur$D4_5<-qsur$D4_11
qsur$D4_6<-qsur$D4_12

print(qsur[,D2_l])
print(qsur[,D4_l])

#CLEANING THE DATA
#Drop speeders (time less than 7 minutes)
qsur$Duration<-qsur$Duration/60
qsur<-qsur[qsur$Duration>=7,]


#flag and drop responses with inconsistent responses to threat levels
qsur$sw7<-0
qsur$secsw7<-0
count<-1
for(v in C7_l) {
  print(table(qsur[,v]))
  qsur$sw7[which((qsur[,v]<4)&(qsur$sw7==0))]<-count
  qsur$secsw7[which((qsur[,v]==5)&(qsur$sw7>0))]<-count
  print(v)
  count<-count+1
}


qsur$sw8<-0
qsur$secsw8<-0
count<-1
for(v in C8_l) {
  print(table(qsur[,v]))
  qsur$sw8[which((qsur[,v]<4)&(qsur$sw8==0))]<-count
  qsur$secsw8[which((qsur[,v]==5)&(qsur$sw8>0))]<-count #ignore don't know answer
  print(v)
  count<-count+1
}

qsur$swD2<-0
qsur$secswD2<-0
count<-1
for(v in D2_l) {
  qsur$swD2[which((qsur[,v]>2)&(qsur[,v]<6)&(qsur$swD2==0))]<-count
  qsur$secswD2[which((qsur[,v]==1)&(qsur$swD2>0))]<-count #ignore don't know answer
  count<-count+1
}


qsur$swD4<-0
qsur$secswD4<-0
count<-1
for(v in D4_l) {
  qsur$swD4[which((qsur[,v]>2)&(qsur[,v]<6)&(qsur$swD4==0))]<-count
  qsur$secswD4[which((qsur[,v]==1)&(qsur$swD4>0))]<-count
  count<-count+1
}


qsur$inconsist<-(qsur$secsw7>0)+(qsur$secsw8>0)+(qsur$secswD2>0)+(qsur$secswD4>0)
table(qsur$inconsist,qsur$group)
qsur<-filter(qsur,inconsist<=1)





###  LABELING ###
qsur$groupn<-qsur$group
qsur$group <- factor(qsur$group,levels = c(2,1),labels = c("Good_English","Lim_English"),ordered=TRUE)
qsur$gender<-factor(qsur$gender,levels = c(1,2),labels = c("Female","Male"),ordered=TRUE)
labels_educ<-c("No school","Grades 1-12, no HS diploma","HS diploma","Some college","Associate or bachelor's degree","Advanced degree")
qsur$educ<-factor(qsur$educ,levels = c(1,2,3,4,5,6),labels = labels_educ,ordered=TRUE)
labels_home<-c("Skip","Apartment/Condo","Well-built house","Well-built townhouse","Mobile home","Manufactured home","Other")
qsur$home<-factor(qsur$home,levels = c(-99,1,2,3,4,5,6),labels = labels_home,ordered=TRUE)
print(table(qsur$group,qsur$gender))
qsur$age_groupf<-factor(qsur$age_group,levels=c(0,1,2),labels=c("<35","35-59","60+"),ordered=TRUE)


#Labeling multiple responses question A1 on ways to receive tornado warnings
FUN<-function(v) recode(v,'-99'=0, '1'=1, .default = 0)
A1q<-names(qsur)[substr(names(qsur),1,2)=="A1"&names(qsur)!="A1_8_TEXT"&substr(names(qsur),1,5)!="A1_DO"]
print(A1q)
qsur[,A1q] <- data.frame(apply(qsur[A1q], 2, FUN))

Info_sources<-c("Weather radio","Local radio","Local TV","Internet-based service","Weather channel","Cell phone app/SMS","Family/friends/neighbors","I usually do not receive warnings","Other")
qsur[,A1q]<-set_label(qsur[,A1q],Info_sources)
print(get_label(qsur[,A1q]))


#Labeling multiple responses to question A2 on probability to receive tornado warnings
A2q<-names(qsur)[substr(names(qsur),1,2)=="A2"]
print(A2q)
FUNA2<-function(v) recode(v,'-99'=NULL,'1'=1,'2'=2,'3'=3,'4'=4,.default = NULL)
qsur[,A2q] <- data.frame(apply(qsur[A2q], 2, FUNA2))

FUN_FA2<-function(v) factor(v,levels = c(1,2,3,4),labels = c("<10%","10-50%","51-75%","76-100%"),ordered=TRUE)
qsur[,A2q] <- data.frame(apply(qsur[A2q], 2, FUN_FA2))

A2_times<-c("Weekday 2am","Weekday 3pm","Weekday 7pm","Weekend 7pm")
qsur[,A2q]<-set_label(qsur[,A2q],A2_times)
print(get_label(qsur[,A2q]))



#Sheltering place (B3)
labels_shelter<-c("Bathroom","Basement","Closet","Kitchen, bedroom or a living room","Hallway","Specifically designed safe room","Tornado shelter (including backyard shelter)","Other area","I would never shelter inside my home","Don't know")
qsur$B3<-factor(qsur$B3, levels=c(1,8,9,2,3,4,5,7,6,10),labels=labels_shelter,ordered=TRUE)
print(table(qsur$B3))

#Window in a sheltering place (B4)
qsur$B4<-factor(qsur$B4, levels=c(5,4),labels=c("No","Yes"),ordered=TRUE)

#Vehicle availability
qsur$vehicle<-recode(qsur$B5,'1'=1,'2'=0,'3'=0,.default=NULL)
qsur$vehicle<-factor(qsur$vehicle, levels=c(0,1),labels=c("No","Yes"),ordered=TRUE)
print(table(qsur$vehicle))

#Labeling multiple responses on question F6 (race)
F6_q<-names(qsur)[substr(names(qsur),1,2)=="F6"]
print(F6_q)
qsur[,F6_q] <- data.frame(apply(qsur[F6_q], 2, FUN))
qsur$race<-1*qsur$F6_1+2*qsur$F6_2+3*qsur$F6_3+4*qsur$F6_4+5*qsur$F6_5+6*qsur$F6_6
multrace<-rowSums(qsur[,F6_q], na.rm = FALSE, dims = 1)>1
qsur$race[multrace==TRUE]<-9
labels_race<-c("White","Black","Asian","Native American","Native Hawaiian","Other","Mixed")
qsur$race<-factor(qsur$race,levels = c(5,3,2,1,4,6,9),labels = labels_race,ordered=TRUE)
print(table(qsur$race))


FUN<-function(v) recode(v,'5'=0, '4'=1, '1'=2, '2'=3, '3'=4,'-99'=NULL)
#Recoding no advance time threat responses:
#0 - Nothing, 1 - Don't know, 2 - take shelter at home, 3 - drive to a shelter, 4 -flee
qsur$xt<-lapply(qsur$C7_4, FUN)
print(cbind(qsur$xt,qsur$C7_4))
qsur[,C7_l] <- data.frame(apply(qsur[C7_l], 2, FUN))
qsur[,C8_l] <- data.frame(apply(qsur[C8_l], 2, FUN))
print(table(qsur$C7_2))

#Recoding advance time threat responses:
#Was: 1 - Nothing, 2 - Seek info, 3- Shelter at home, 4 - Drive to a shelter, 5 - flee, 6 - don't know
#now: the same as is C7, C8 above, but 5 = Seek more info
print(table(qsur$D2_4))
FUN<-function(v) recode(v,'1'=0,'6'=1, '3'=2, '4'=3, '5'=4, '2'=5, .default=NULL)
qsur$xt<-lapply(qsur$D2_4, FUN)
print(cbind(qsur$xt,qsur$D2_4))

qsur[,D2_l] <- data.frame(apply(qsur[D2_l], 2, FUN))
qsur[,D4_l] <- data.frame(apply(qsur[D4_l], 2, FUN))
print(table(qsur$D2_3))



#Labeling C1
labels_C1<-c("Skip","Nothing","Seek more information without taking shelter","Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path","Other")

qsur$C1<-factor(qsur$C1,levels = c(-99,1,2,3,4,5,6),labels = labels_C1,ordered=TRUE)
qsur$C2<-factor(qsur$C2,levels = c(-99,1,2,3,4,5,6),labels = labels_C1,ordered=TRUE)
qsur$D1<-factor(qsur$D1,levels = c(-99,1,2,3,4,5,6),labels = labels_C1,ordered=TRUE)

#Labeling protection vars
protectC1<-factor(protectC1,levels = c(0,1),labels = c("No action","Take action"),ordered=TRUE)
protectD1<-factor(protectD1,levels = c(0,1),labels = c("No action","Take action"),ordered=TRUE)

protectC1s<-factor(protectC1s,levels = c(0,1),labels = c("Other","Protect"),ordered=TRUE)
protectD1s<-factor(protectD1s,levels = c(0,1),labels = c("Other","Protect"),ordered=TRUE)

table(protectC1,protectD1)
table(protectC1s,protectD1s)

#Labeling C7 and C8
#FUN2<-function(v) factor(v,levels = c(-99,0,1,2,3,4),labels = c("Skip","Nothing","Don't know","Shelter at home","Drive to a shelter","Flee"),ordered=TRUE)
#qsur[,C7_l] <- data.frame(lapply(qsur[C7_l], FUN2))
#qsur[,C8_l] <- data.frame(lapply(qsur[C8_l], FUN2))



#Labeling the threat responses:


labels_threat<-c("0%","10%","20%","40%","60%","100%","Never")
#qsur$sw7<-recode(qsur$sw7,'1'=0,'2'=0.1,'3'=0.2,'4'=0.4,'5'=0.6,'6'=1.0, default=NULL)
qsur$sw7<-factor(qsur$sw7,levels = c(1,2,3,4,5,6,0),labels = labels_threat,ordered=TRUE)
qsur$sw8<-factor(qsur$sw8,levels = c(1,2,3,4,5,6,0),labels = labels_threat,ordered=TRUE)
qsur$swD2<-factor(qsur$swD2,levels = c(1,2,3,4,5,6,0),labels = labels_threat,ordered=TRUE)
qsur$swD4<-factor(qsur$swD4,levels = c(1,2,3,4,5,6,0),labels = labels_threat,ordered=TRUE)

saveRDS(qsur,file="../Input/qsur_clean.Rdata")

