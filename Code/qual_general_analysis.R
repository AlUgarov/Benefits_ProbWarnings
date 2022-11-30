

remove(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(sjlabelled)
library(RColorBrewer)
library(magrittr)
library(questionr) #weighted tables
library(data.table)
library(srvyr)


qsur<-readRDS(file="../Input/qsur_clean.Rdata")

qsur<-droplevels(qsur)



#-DETERMINISTIC WARNING COMPARISON: STANDARD VS EXTENDED LEAD TIMES---
#C1 vs D1

#Protective response variables:
protect_responses<-c("Nothing","Seek more information without taking shelter","Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path","Other")

#Prepare the data for the comparison graph of protective responses

#prepare the data for the paired t-tests:

#Create a vector of differences between positions (used to plot standard vs extended graph)
labels_C1<-c("Nothing","Seek more information without taking shelter","Take shelter at home or near","Drive to another house or structure","Drive out of the potential tornado path","Other")

qsur %>% select(ResponseId, pweight, C1, D1) %>% filter(!is.na(C1)&!is.na(D1))->comp_resp_data

diff<-model.matrix( ~ D1 - 1, data=comp_resp_data)-model.matrix( ~ C1 - 1, data=comp_resp_data)
diff<-as.data.frame(diff)
names(diff)<-c("diff_1", "diff_2", "diff_3","diff_4","diff_5","diff_6")
comp_resp_data<-cbind(comp_resp_data,diff)
print(comp_resp_data)

comp_resp_data %>% pivot_longer(cols=diff_1:diff_6, names_to="varname", values_to="diff") %>%
  mutate(response=substr(varname,6,6)) %>%
  select(ResponseId, pweight, response, diff) %>%
  as_survey_design(ids=ResponseId, weights=pweight) %>%
  group_by(response) %>% 
  summarise(diff=survey_mean(diff, vartype=c("ci"), na.rm=TRUE, level = c(0.95,0.99))) %>%
  mutate(lab_diff=case_when((diff_low99>0)~" (>**)",diff_upp99<0~" (<**)",diff_low95>0~" (>*)",diff_upp95<0~" (<*)", TRUE~"")) %>%
  mutate(Lead_time="extended")%>%
  select(Lead_time, response, lab_diff)->prot_tests


#calculate props of choosing each protective response
comp_resp_data %>% pivot_longer(cols=C1:D1, names_to="varname", values_to="choice") %>%
  filter(!choice=="Skip") %>%
  mutate(Lead_time=case_when(substr(varname,1,1)=="C"~"standard", substr(varname,1,1)=="D"~"extended"))->dat1

as.data.frame(prop.table(wtd.table(dat1$Lead_time,dat1$choice,weights=dat1$pweight),1)) %>%
  filter(!Var2=="Skip") %>% rename(Lead_time = Var1, responsef=Var2, prop=Freq) %>%
  mutate(response=recode(responsef, "Nothing"=1,"Seek more information without taking shelter"=2,
                         "Take shelter at home or near"=3, "Drive to another house or structure"=4,
                         "Drive out of the potential tornado path"=5, "Other"=6)) ->prot_means
print(prot_means)

prot_means<-merge(prot_means,prot_tests,by=c("Lead_time","response"),all.x = TRUE)
print(prot_means)

prot_means %>% mutate(lab_diff=case_when(Lead_time=="standard" ~"", TRUE ~ lab_diff)) %>%
  mutate(lab=paste(as.character(format(100*prop,digits=0)),"%",lab_diff, sep="")) %>%
  mutate(Lead_time=recode(Lead_time,'standard'="Standard", 'extended'="Extended (40 min)")) %>%
  mutate(response=factor(response, levels = c(1,2,3,4,5,6),labels = labels_C1,ordered=TRUE))->prot_means

#graph to compare response rates by time and lead times:
contr_plot<-prot_means%>%
  ggplot(aes(x = response, y=prop, fill=Lead_time, group=Lead_time))+
  geom_bar(stat="identity", position=position_dodge(), na.rm=TRUE)+
  #scale_fill_brewer(palette="Set1", guide= guide_legend(reverse = TRUE, title="Lead time:"))+
  scale_fill_manual(values = c("red","navy"),guide= guide_legend(reverse = TRUE, title="Lead time:"))+
  coord_flip()+
  labs(x="Response", y="% mentioned", title = "What would you do? \n(tornado warning at home at 7 PM)", caption = "Internet-sample, weighted")+
  geom_text(aes(label = lab), position=position_dodge(width=0.9), hjust = -.2)+
  scale_y_continuous(limits=c(0,1),labels = scales::percent) +
  theme(text = element_text(size=15))+
  scale_x_discrete(limits = rev(labels_C1))

print(contr_plot)
ggsave("../Graphs/contr_plot_allQ.pdf",width = 10, height = 6)



#Probabilistic warnings now:
#Cumulative distribution of threat responses by probability (all respondents):
#Lists of variable names handles (used both in labeling and cleaning)
C7_l<-names(qsur)[substr(names(qsur),1,2)=="C7"]
C8_l<-names(qsur)[substr(names(qsur),1,2)=="C8"]


D2_l<-c("D2_1","D2_2","D2_3","D2_4","D2_5","D2_6")
D4_l<-c("D4_1","D4_2","D4_3","D4_4","D4_5","D4_6")

print(C7_l)
print(C8_l)
print(D2_l)
print(D4_l)

qsur$housing="Permanent"
qsur$housing[!is.na(qsur$home)&qsur$home=="Mobile home"]="Mobile"

threat_levels<-c("0%","10%","20%","40%","60%","100%")


#proportions switching for each group:
qsur %>% select(ResponseId, pweight, housing, C7_l,C8_l,D2_l,D4_l) %>%
  pivot_longer(cols="C7_1":"D4_6", names_to="varname", values_to="response") %>%
  filter(response!=-99&!is.na(response)) %>%
  mutate(shelter=case_when((housing=="Permanent"&response %in% c(2,3,4)) ~ 1, (housing=="Mobile"&response %in% c(3,4)) ~ 1, TRUE ~0)) %>%
  mutate(time=case_when(substr(varname,2,2) %in% c("7","2") ~ "7 PM",
                        substr(varname,2,2) %in% c("8","4")  ~ "2 AM")) %>%
  mutate(Lead_time=case_when(substr(varname,1,1)=="C" ~ "standard lead time",
                             substr(varname,1,1)=="D"  ~ "extended lead time")) %>%
  mutate(prob=as.numeric(substr(varname,4,4))) %>%
  mutate(prob=recode(prob,'1'=0,'2'=0.1,'3'=0.2,'4'=0.4,'5'=0.6,'6'=1)) %>%
  as_survey_design(ids=ResponseId, weights=pweight) %>%
  group_by(time, Lead_time, prob) %>%
  summarise(nobs=n(), prot_share=survey_mean(shelter, proportion = TRUE, vartype=c("ci"), na.rm=TRUE)) %>%
  arrange(time, Lead_time, prob) %>%
  as.data.frame() ->threat_resp
print(threat_resp)


#Graph of probabilistic responses:
threatresp_plot<-threat_resp %>%
  ggplot(aes(x = prob, y=prot_share, group=Lead_time, color=Lead_time))+
  geom_line(aes(x = prob, y=prot_share, group=Lead_time, color=Lead_time), size=2)+
  scale_color_manual(values = c("navy","red"),guide= guide_legend(reverse = TRUE))+
  #scale_color_brewer(palette="Set1", direction=-1)+
  geom_ribbon(aes(ymin = prot_share_low, ymax = prot_share_upp), alpha = 0.2, colour = NA)+
  #geom_area(mapping = aes(x = Threat, y=Protect, fill=Lead_Time, group=Lead_Time), position="identity", na.rm=TRUE)+
  scale_x_continuous(limits = c(0,1),expand=c(0,0.1),labels = scales::percent)+
  scale_y_continuous(limits=c(0,1),labels = scales::percent)+
  theme(text = element_text(size=15))+
  labs(x="Risk level (%)", y="", title = "Proportion of respondents choosing to protect \n(Internet-sample)")

threatresp_plot_all<-threatresp_plot + facet_wrap(vars(time))+ theme(legend.position="bottom",legend.title=element_blank())
print(threatresp_plot_all)

ggsave("../Graphs/threat_resp_allQ.pdf", width=10, height=6)
ggsave("../Graphs/threat_resp_allQ.png", device=png(), width=10, height=6, dpi=600)






