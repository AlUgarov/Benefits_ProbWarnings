
remove(list = ls())
library(dplyr)
library(reshape2)
library(xtable)
library(ggplot2)
library(tidyr)
library(sjlabelled)
library(survey)
library(srvyr)
library(RColorBrewer)
library(magrittr)


#Put your own data folder here:
setwd("C:/Tornado_warnings/Benefits_ProbWarnings")

#Import pre-processed mail survey data:
qsur<-readRDS(file="./Temp/Ext_Alerts_Merged.Rdata")

#Loading prepared ACS sample descr stats:
load(file = "./Input/ACS_descrstat_all.Rdata")
load(file = "./Input/ACS_descrstat_sep.Rdata")


#-----DESCRIPTIVE STATS---------------------
#First tornado-prone areas only (no differentiation btw tornado-prone and not)
tab_gender<-as.data.frame(prop.table(table(qsur$gender)))
tab_age<-as.data.frame(prop.table(table(qsur$age_groupf)))
tab_educ<-as.data.frame(prop.table(table(qsur$educ)))

descr_statp<-rbind(tab_gender,tab_age,tab_educ)
descr_statn<-rbind(as.data.frame(table(qsur$gender)),as.data.frame(table(qsur$age_groupf)),as.data.frame(table(qsur$educ)),as.data.frame(table(qsur$race)))
descr_stat<-cbind(descr_statn[-1,],100*descr_statp[-1,2],descr_stat0[,1])
colnames(descr_stat)<-c("Group","Sample (N)","Sample (%)","Population (%)")
print(descr_stat)

write.csv(descr_stat, file = "./Tables/descr_stat_all.csv")

print(xtable(descr_stat, type = "latex",digits=c(0)), file = "./Tables/descr_stat_all.tex")

#Adding Qualtrics data: (should I do it?)
load(file = "./Input/Qualt_descrstat_all.Rdata")
descr_stat<-cbind(descr_stat[,-4],descr_statQ[,c(2,3)],descr_stat[,-c(1,2,3)])
colnames(descr_stat)<-c("Group","Sample (N)","Sample (%)","Sample (N)","Sample (%)","Population (%)")
print(descr_stat)




#Now separately for tornado-prone and other states:
tab_gender<-prop.table(table(qsur$gender,qsur$tornadopronef),2)
tab_age<-prop.table(table(qsur$age_groupf,qsur$tornadopronef),2)
tab_educ<-prop.table(table(qsur$educ,qsur$tornadopronef),2)



descr_statp<-rbind(tab_gender,tab_age,tab_educ)
descr_statn<-rbind(table(qsur$gender,qsur$tornadopronef),table(qsur$age_groupf,qsur$tornadopronef),table(qsur$educ,qsur$tornadopronef))
descr_stat<-cbind(descr_statn[-1,0], descr_statn[-1,1],100*descr_statp[-1,1],descr_stat_sep[,1],descr_statn[-1,2],100*descr_statp[-1,2],descr_stat_sep[,2])
colnames(descr_stat)<-c("Sample (N)","Sample (%)","Population (%)","Sample (N)","Sample (%)","Population (%)")
print(descr_stat)

write.csv(descr_stat, file = "./Tables/descr_stat_sep.csv")

print(xtable(descr_stat, type = "latex",digits=c(0)), file = "./Tables/descr_stat_sep.tex")



# probabilistic risk perception question:
C5_l<-names(qsur)[substr(names(qsur),1,3)=="C5-"&substr(names(qsur),8,11)!="TEXT"]


#Protective response variables:
C1_l<-names(qsur)[substr(names(qsur),1,2)=="C1"&names(qsur)!="C1_7_TEXT"&names(qsur)!="C1"]
C2_l<-names(qsur)[substr(names(qsur),1,2)=="C2"&names(qsur)!="C2_7_TEXT"&names(qsur)!="C2"]
D1_l<-names(qsur)[substr(names(qsur),1,2)=="D1"&names(qsur)!="D1_6_TEXT"&names(qsur)!="D1"]
D2_l<-names(qsur)[substr(names(qsur),1,2)=="D2"&names(qsur)!="D2_6_TEXT"&names(qsur)!="D2"]

protect_responses<-c("Nothing","Seek more information without taking shelter","Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path","I would never learn about the warning at that time", "Other")

#Prepare the data for the comparison graph of protective responses between
#standard and extended warning times




#prepare the data for the paired t-tests:
qsur[,c("AccessCode","selprob", "weight_a", C1_l,C2_l,D1_l,D2_l)] %>% pivot_longer(cols=C1_1:D2_7, names_to="varname", values_to="choice") %>%
  mutate(time=case_when(substr(varname,2,2)=="1"~"day", substr(varname,2,2)=="2"~"night")) %>%
  mutate(Lead_time=case_when(substr(varname,1,1)=="C"~"stand_resp", substr(varname,1,1)=="D"~"ext_resp")) %>%
  mutate(response=substr(varname,4,4)) %>%
  select(AccessCode, selprob, response, Lead_time, time, choice, weight_a) %>%
  pivot_wider(names_from=c(Lead_time),values_from=c(choice)) %>%
  filter(is.na(stand_resp)==FALSE&is.na(ext_resp)==FALSE) %>%
  mutate(diff=ext_resp-stand_resp, selprob=pmin(selprob,0.0002)) %>%
  filter(response!=6)%>%
  mutate(weight=1/selprob)->dat0

options(survey.lonely.psu="remove")

#make paired t-tests and create text labels with the results
dat0 %>% as_survey_design(ids=AccessCode, weights=weight_a) %>%
  group_by(response, time) %>% 
  summarise(diff=survey_mean(diff, vartype=c("ci"), na.rm=TRUE, level = c(0.95,0.99))) %>%
  mutate(lab_diff=case_when((diff_low99>0)~" (>**)",diff_upp99<0~" (<**)",diff_low95>0~" (>*)",diff_upp95<0~" (<*)", TRUE~"")) %>%
  mutate(Lead_time="extended")%>%
  select(time, Lead_time, response, lab_diff)->prot_tests

#calculate props of choosing each protective response
dat0 %>% pivot_longer(cols=stand_resp:ext_resp, names_to="varname", values_to="choice")%>%
  mutate(Lead_time=recode(varname, `stand_resp`="standard",`ext_resp`="extended")) %>%
  select(AccessCode, selprob, response, Lead_time, time, choice, weight_a) %>%
  as_survey_design(ids=AccessCode, weights=weight_a) %>%
  group_by(time, Lead_time, response) %>%
  summarise(prop=survey_mean(choice, vartype=c("se"), na.rm=TRUE)) %>%
  arrange(time, Lead_time, response)->prot_means

#merging test results with the data and final modifications for the graph:
prot_means=merge(prot_means,prot_tests,by=c("time", "Lead_time", "response"),all.x=TRUE)
prot_means %>% mutate(lab_diff=case_when(Lead_time=="standard" ~"", TRUE ~ lab_diff)) %>%
  mutate(lab=paste(as.character(format(100*prop,digits=0)),"%",lab_diff, sep="")) %>%
  mutate(time=recode(time, 'day'="7 PM",'night'="2 AM"))%>%
  mutate(Lead_time=recode(Lead_time,'standard'="Standard", 'extended'="Extended (40 min)")) %>%
  mutate(response=factor(response, levels = c(1,2,3,4,5,7),labels = protect_responses[-6],ordered=TRUE))->prot_means

#graph to compare response rates by time and lead times:
contr_plot<-prot_means%>%
  ggplot(aes(x = response, y=prop, fill=Lead_time, group=Lead_time))+
  geom_bar(stat="identity", position=position_dodge(), na.rm=TRUE)+
  #scale_fill_brewer(palette="Set1", guide= guide_legend(reverse = TRUE, title="Lead time:"))+
  scale_fill_manual(values = c("red","navy"),guide= guide_legend(reverse = TRUE, title="Lead time:"))+
  coord_flip()+
  labs(x="Response", y="% mentioned", title = "What would you do? (tornado warning at home)")+
  geom_text(aes(label = lab), position=position_dodge(width=0.9), hjust = -.2)+
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  theme(text = element_text(size=15))+
  scale_x_discrete(limits = rev(protect_responses[-6]))


contr_plots<-contr_plot+ facet_wrap(vars(time))+theme(legend.position="bottom")
print(contr_plots)
ggsave("./Graphs/comp_responses_w.pdf",width = 10, height = 6)




#Calculate and save responses to threats:
prot_responsesP=c("Seek more information without taking shelter","Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path")
prot_responsesM=c("Drive to another house or structure","Drive out of the potential tornado path", "Other")

shelter_responsesP=c("Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path")
shelter_responsesM=prot_responsesM


qsur$housing="Permanent"
qsur$housing[!is.na(qsur$home)&qsur$home=="Mobile home"]="Mobile"

qsur$housing<-as.factor(qsur$housing)
qsur$pweight=qsur$weight_a
qsur %>% select(AccessCode, housing, pweight, C1, C2, D1, D2) %>%
  pivot_longer(cols=C1:D2, names_to="varname", values_to="response") %>%
  mutate(time=case_when(substr(varname,2,2)=="1"~"day", substr(varname,2,2)=="2"~"night")) %>%
  mutate(warn_type=case_when(substr(varname,1,1)=="C"~"standard", substr(varname,1,1)=="D"~"extended")) %>%
  mutate(protect=case_when((housing=="Permanent"&response %in% prot_responsesP) ~ 1, (housing=="Mobile"&response %in% prot_responsesM) ~ 1, TRUE ~0)) %>%
  mutate(shelter=case_when((housing=="Permanent"&response %in% shelter_responsesP) ~ 1, (housing=="Mobile"&response %in% shelter_responsesM) ~ 1, TRUE ~0)) %>%
  as_survey_design(ids=AccessCode, weights=pweight) %>%
  group_by(time, warn_type, housing) %>%
  summarise(prop=survey_mean(protect, vartype=c("se"), na.rm=TRUE), shelter=survey_mean(shelter, vartype=c("se"), na.rm=TRUE)) %>%
  arrange(housing, time, warn_type) %>%
  as.data.frame() %>% write.csv('./Output/det_props_mail.csv')



#Probabilistic warnings now:
#question C5
C5_l<-names(qsur)[substr(names(qsur),1,3)=="C5."&substr(names(qsur),8,11)!="TEXT"]
threat_levels<-c("0%","20%","40%","60%","80%","100%")

#adjusting responses by house type:
qsur$swC5a<-0 #only actual protection
#qsur$swC5b<-0 #protection and watching (except mobile homes where people protect only by evacuating)

count<-1

for(v in C5_l) {
  print(table(qsur[,v]))
  qsur$swC5a[which((qsur$housing=="Permanent")&(qsur[,v]>1)&(qsur[,v]<5)&(qsur$swC5a==0))]<-count
  qsur$swC5a[which((qsur$housing=="Mobile")&(qsur[,v]>2)&(qsur[,v]<5)&(qsur$swC5a==0))]<-count
  #qsur$swC5b[which((qsur$housing=="Permanent")&(qsur[,v]>1)&(qsur[,v]<6)&(qsur$swD2b==0))]<-count
  #qsur$swC5b[which((qsur$housing=="Mobile")&(qsur[,v] %in% c(4,5))& (qsur$swD2b==0))]<-count
  count<-count+1
}


labels_threat<-c(threat_levels,"Never")
qsur$threschold<-factor(qsur$swC5a,levels = c(1,2,3,4,5,6,0),labels = labels_threat,ordered=TRUE)

table(qsur$threschold)

#proportions switching for each group:
qsur %>% filter(secsw5==0) %>% select(housing, pweight, threschold) %>%
  group_by(housing, threschold) %>% arrange(housing, threschold) %>% 
  summarise(totw=sum(pweight)) %>%   mutate(totsw=cumsum(totw), totN=sum(totw)) %>%
  mutate(prot_share=totsw/totN) %>% select(housing, threschold, prot_share) %>%
  filter(threschold!="Never") %>% mutate(time="7 PM") %>%
  arrange(housing, threschold) %>%
  as.data.frame() -> prob_props


write.csv(prob_props, './Output/prob_props_mail.csv')

#proportions switching for each group:
qsur %>% filter(secsw5==0) %>% select(AccessCode, pweight, housing, C5_l) %>%
  pivot_longer(cols="C5.1":"C5.6", names_to="prob", values_to="response") %>%
  filter(response!=-99) %>%
  mutate(shelter=case_when((housing=="Permanent"&response %in% c(2,3,4)) ~ 1, (housing=="Mobile"&response %in% c(3,4)) ~ 1, TRUE ~0)) %>%
  as_survey_design(ids=AccessCode, weights=pweight) %>%
  group_by(prob) %>%
  summarise(nobs=n(), prot_share=survey_mean(shelter, proportion = TRUE, vartype=c("ci"), na.rm=TRUE)) %>%
  mutate(prob=factor(prob,levels=C5_l, labels=threat_levels, ordered=TRUE)) %>%
  arrange(prob) %>%
  as.data.frame() ->threat_resp


#Graph of probabilistic responses:
threatresp_plot<-threat_resp %>%
  ggplot(aes(x = prob, y=prot_share, group = 1))+
  geom_line(size=2, colour="navy")+
  geom_ribbon(aes(ymin = prot_share_low, ymax = prot_share_upp), alpha = 0.1)+
  #geom_area(mapping = aes(x = Threat, y=Protect, fill=Lead_Time, group=Lead_Time), position="identity", na.rm=TRUE)+
  scale_x_discrete(limits = threat_levels,expand=c(0,0.1))+
  scale_y_continuous(limits=c(0,1),labels = scales::percent)+
  theme(text = element_text(size=15))+
  labs(x="Risk level (%)", y="", title = "Proportion of respondents choosing to protect (6 PM)")

print(threatresp_plot)
ggsave("./Graphs/threat_resp_mail_w.pdf",width = 10, height = 6)
