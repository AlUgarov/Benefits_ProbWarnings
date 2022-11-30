remove(list = ls())
library(dplyr)
library(xtable)
library(haven)
library(tidyr)
library(sjlabelled)



#Put your own data folder here:
require("haven")

#Put your own data folder here:
setwd("C:/Tornado_warnings/Benefits_ProbWarnings")

qsur <- read.csv(file = './Input/Pilot_Data.csv')
head(qsur)

qsur$StartDate<-as.Date(qsur$StartDate, format = "%m/%d/%y")

#RENAMING
names(qsur)[names(qsur) == "F1"] <- "gender"
names(qsur)[names(qsur) == "F6"] <- "hisp_origin"
names(qsur)[names(qsur) == "F9"] <- "educ"
names(qsur)[names(qsur) == "B1"] <- "home"
names(qsur)[substr(names(qsur),1,5) == "Durat"] <- "Duration"



qsur$F2_1[qsur$F2_1==-99]<-NA
qsur$birthyear<-qsur$F2_1+1930-2
qsur$age<-2020-qsur$birthyear
qsur$age_group<-cut(qsur$age,c(0,34,59,Inf))
qsur$age_group<-recode(qsur$age_group,'(0,34]'=0,'(34,59]'=1,'(59,Inf]'=2)
qsur$age_groupf<-factor(qsur$age_group,levels=c(0,1,2),labels=c("<35","35-59","60+"),ordered=TRUE)
print(table(qsur$age_groupf))


qsur$Income<- recode(qsur$F10,'-99'=NA_real_, '1'=5000, '2'=17500, '3'=37500, '4'=62500, '5'=87500, '6'=112500, '7'=137500, '8'=175000,'9'=NULL)
qsur$logincome<-log(qsur$Income)
qsur$goodhouse<-recode(qsur$home,'-99'=NULL,'2'=1,'3'=1,.default=0)
table(qsur$Income)

table(qsur$F3)
qsur$hhsize=qsur$F3
qsur$hhsize[qsur$F3==-99]=NA
qsur$hhsize[qsur$F3==100]=NA
table(qsur$hhsize)

A1q<-names(qsur)[substr(names(qsur),1,2)=="A1"&names(qsur)!="A1_8_TEXT"&substr(names(qsur),1,5)!="A1_DO"]
print(A1q)

C1_l<-names(qsur)[substr(names(qsur),1,2)=="C1"&names(qsur)!="C1_7_TEXT"]
C2_l<-names(qsur)[substr(names(qsur),1,2)=="C2"&names(qsur)!="C2_7_TEXT"]


qsur$pilot<-1
basevars<-c("pilot","Status","Finished","UserLanguage","AccessCode","ExternalDataReference")
demvars<-c("gender","age","age_group","age_groupf","hhsize","Income","logincome")

keepnames<-c(basevars, A1q,C1_l,C2_l,"E1","E2", "E3_1",demvars)

qsur_pilot<-qsur[,keepnames]
head(qsur_pilot)
saveRDS(qsur_pilot,file="./Temp/Ext_Alerts_Pilot.Rdata")
