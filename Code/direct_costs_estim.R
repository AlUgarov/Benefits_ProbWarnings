#calculate direct costs and benefits of tornado alerts

remove(list = ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(xtable)
tempdir()
dir.create(tempdir())



##-- ASSUMPTIONS --

#Calculate population density in the area
states_dat<-read.csv(file = '../Input/sel_states.csv')
density=sum(states_dat$Population)/sum(states_dat$Area)
print(density)

#Average warning area:
warn_area=275 # from Howard et al (2020), in sq. miles

VSL=11.13 #in mln USD 2020

#N of warnings:
N_warnings=2063 #from Howard et al (2020)

#False alarm rate:
FAR=0.7 #from Howard et al (2020)

#Probabiity of detection:
POD=0.7

#strike_rate=0.01 #need to adjust
strike_area=0.302 #from Simmons and Sutter (2011) based on authors' calculation for 1990-2002
#strike_area=0.798 #based on my calc for 2000-2016
#strike_area=3.6*0.302

rel_area=pi*10^2

strike_rateP=strike_area/rel_area 
strike_rate=strike_area/warn_area

#Average annual population under warnings:
Pop_warned=N_warnings*warn_area*density

print("Population warned is:")
print(Pop_warned)


#Average annual population in the strike zone of tornadoes:
Pop_affected=((1-FAR)/POD)*N_warnings*strike_area*density


#Costs of time:
time_price=10 #dollars per hour for all population

#Note: time price can be lower for some populations in mobile houses, or for limited English Hispanics

warn_duration=37/60 #from Brooks and Correia (2018)



#---CALCULATION OF MITIGATION EFFICIENCY AND BASELINE RISKS---
# starting from the equations in the paper--
pop_M=0.07
pop_P=1-pop_M
warn_effect=0.35 #based on Simmons and Sutter (2005, 2009)
r_M_av=0.008472 #average fatality rate, mobile
r_P_av=0.000882 #average fatality rate, permanent
R_M=0.3 #protective response, mobile
R_P=0.6 #protective response, permanent
#First, calculate for mobile homes:
# Note r_M_av=base_rate*[(1-POD)+POD*(R*m+(1-R))], m=0 for mobile homes
m_M=0
r_M_b=r_M_av/((1-POD)+POD*(R_M*m_M+(1-R_M)))
r_M_w=r_M_b*(R_M*m_M+(1-R_M))
print(c(r_M_b,r_M_w))

#Second, calculate for permanent homes by solving two equations:
#r_P_w-(1-warn_effect)*r_P_b=(pop_M/pop_P)*(r_M_w-(1-warn_effect)*r_B_M)=V
#(1-POD)*r_P_B+POD*r_P_w=r_P_av
V=(pop_M/pop_P)*(r_M_w-(1-warn_effect)*r_M_b)
#r_P_b=(r_P_av-POD*V)/(1-POD+POD*(1-warn_effect))
r_P_b=(r_P_av+POD*V)/(1-POD*warn_effect)
r_P_w=(1-warn_effect)*r_P_b-V
print(c(r_P_b,r_P_w))
print(1-r_P_w/r_P_b)

#Now accounting for compliance:
m_P=1-(1/R_P)*(r_P_b-r_P_w)/r_P_b
print(m_P)

#Calculate the average warning effect again for verification:
print((pop_P*r_P_w+pop_M*r_M_w)/(pop_P*r_P_b+pop_M*r_M_b))


#baseline injury rates are in the same proportion to average rate as fatality baseline to average:
inj_rate=c((r_P_b/r_P_av)*0.0224,(r_M_b/r_M_av)*0.025)

#Baseline injury and fatality rates:
cas_rate=data.frame(housing=c("Permanent","Mobile"), inj_rate, fat_rate=c(r_P_b, r_M_b))

#Population proportions by housing type:
pop_props=data.frame(housing=c("Permanent","Mobile"), pop_props=c(0.9,0.1))

#Population proportions (old):
pop_props_old=data.frame(housing=c("Permanent","Mobile"), pop_props_old=c(0.93,0.07))



#Probabilistic forecast distribution:
prob_distr<-data.frame(threschold=c("0%","20%","40%","60%","80%","100%"),prob_f=c(.7973,0.1243,0.059,0.0125,0.0068,0.0001))

#Mitigation factors:
mitigation_factors=data.frame(housing=c("Permanent","Mobile"), mit_factor=c(m_P,m_M))


#Importing protective responses (table with % sheltering/evacuating for standard and extended warnings)
#note: only evacuation counts as a protective response for mobile housing

read.csv(file = '../Output/det_props_mail.csv')%>% filter(warn_type=="standard") %>%
  select(housing, time, prop, shelter) %>% rename(protect=prop)-> prot_response
read.csv(file = '../Output/prob_props_mail.csv') %>% mutate(time=recode(time, "7 PM"="day", "2 AM"="night")) -> prot_response_prob


#Merging to calculate the mitigation factors:
mit_calculationP<-merge(prot_response_prob,prot_response,by=c("housing","time"),all.x = TRUE)
mit_calculationP<-merge(mit_calculationP,cas_rate,by=c("housing"),all.x = TRUE)
mit_calculationP<-merge(mit_calculationP,mitigation_factors,by=c("housing"),all.x = TRUE)
mit_calculationP<-merge(mit_calculationP,pop_props,by=c("housing"),all.x = TRUE)
mit_calculationP<-merge(mit_calculationP,pop_props_old,by=c("housing"),all.x = TRUE)
mit_calculationP<-merge(mit_calculationP,prob_distr,by=c("threschold"),all.x = TRUE)

POD_prob=1-0.0463*prob_distr[1,2]/0.1


print(mit_calculationP)



#calculate expected fatalities and injuries under standard warnings:
mit_calculationP %>% filter(threschold=="0%") %>%
  mutate(mit_efficiency=(protect*mit_factor+(1-protect))) %>%
  mutate(exp_injuries=pop_props*Pop_affected*inj_rate*mit_efficiency) %>%
  mutate(exp_fatalities=pop_props*Pop_affected*fat_rate*mit_efficiency) %>%
  mutate(shelter_time=pop_props*Pop_warned*shelter*warn_duration) %>%
  mutate(time_costs=shelter_time*time_price) -> det_calculation

det_calculation %>% group_by(time) %>% 
  summarise(sum_injuries=sum(exp_injuries), sum_fatalities=sum(exp_fatalities), sum_timecosts=10^(-6)*sum(time_costs)) %>%
  mutate(costs_fatalities=VSL*sum_fatalities,costs_injuries=0.01*VSL*sum_injuries) %>%
  mutate(totalcosts=costs_fatalities+costs_injuries+sum_timecosts, warn_type="Standard") %>%
  select(warn_type, time, sum_fatalities, sum_injuries, costs_fatalities, costs_injuries, sum_timecosts, totalcosts) %>%
  t()%>%as.data.frame()->warnings_estim
 


#calculate expected fatalities and injuries with no warnings:
det_calculation %>% mutate(base_injuries=exp_injuries/mit_efficiency) %>%
                    mutate(base_fatalities=exp_fatalities/mit_efficiency) %>%
                    group_by(time)%>%summarise(sum_injuries=sum(base_injuries), sum_fatalities=sum(base_fatalities))%>%
                    mutate(costs_fatalities=VSL*sum_fatalities, costs_injuries=0.01*VSL*sum_injuries, sum_timecosts=0)%>%
                    mutate(warn_type="No warning", totalcosts=costs_fatalities+costs_injuries+sum_timecosts) %>%
                    select(warn_type, time, sum_fatalities, sum_injuries, costs_fatalities, costs_injuries, sum_timecosts, totalcosts)%>%
                    t()%>%as.data.frame()->nowarning_estim     


#averaging across 100% deterministic and no warning accounting for POD:
warnings_estim<-cbind(nowarning_estim,warnings_estim)
names(warnings_estim)<-c("nowarn_costs","warn_costs")
warnings_estim %>% tibble::rownames_to_column('cost') %>% slice(3:n()) %>% mutate(warn_costs=as.numeric(warn_costs), nowarn_costs=as.numeric(nowarn_costs)) %>%
  mutate(warn_costs=nowarn_costs*(1-POD)+warn_costs*POD)->warnings_estim
print(warnings_estim)

Pop_universe=(1/0.1)*Pop_affected/strike_rateP

#Now for the probabilistic forecast:
#exp_fatalities - fatalities assuming the same prob for all warnings (prob cancels out)
#wexp_fatalities - fatalities adjusted by probability (assuming average coverage by each probability warning)
mit_calculationP %>% dplyr::rename(prob=threschold) %>% 
  mutate(prob_shift=recode(prob,"0%"=0,"20%"=0.0463,"40%"=0.2,"60%"=0.4,"80%"=0.6, "100%"=0.8))%>%
  mutate(prob=recode(prob,"0%"=0.0463,"20%"=0.2,"40%"=0.4,"60%"=0.6,"80%"=0.8, "100%"=1))%>% #to impute lower bound opp costs
  mutate(resp_share=prot_share) %>% #no wait and see option here
  mutate(mit_efficiency=(resp_share*mit_factor+(1-resp_share))) %>%
  mutate(exp_injuries=prob*prob_f*pop_props*(1/0.1)*Pop_affected*inj_rate*mit_efficiency) %>% #injuries and fat assuming all warnings come with that probability
  mutate(exp_fatalities=prob*prob_f*pop_props*(1/0.1)*Pop_affected*fat_rate*mit_efficiency) %>%
  mutate(pop_warned_prob=prob_f*pop_props*Pop_universe) %>%
  mutate(pot_shelter_time=prob_f*pop_props*Pop_universe*warn_duration) %>%
  mutate(shelter_time=prot_share*pot_shelter_time) %>%
  mutate(time_costs=shelter_time*time_price) %>%
  arrange(housing, time, prob)->aggr_prob_forecast

#Need to find a proportion of people switching at each probability
#First step - add protection share for the previous prob level
n<-dim(aggr_prob_forecast)[1]
aggr_prob_forecast$lag_prot_share<-c(0,aggr_prob_forecast$prot_share[1:(n-1)])
aggr_prob_forecast$lag_prot_share[aggr_prob_forecast$prob<0.4]<-0


#Calculate the implied heterogeneous opportunity costs of time and population proportions:
aggr_prob_forecast %>% mutate(mon_risk=prob_shift*(1-mit_factor)*(VSL*fat_rate+0.01*VSL*inj_rate)*strike_rateP) %>%
                       mutate(mon_risk2=prob*(1-mit_factor)*(VSL*fat_rate+0.01*VSL*inj_rate)*strike_rateP) %>%
                       mutate(value_time=(10^6)*mon_risk/warn_duration) %>%
                       mutate(value_time2=(10^6)*mon_risk2/warn_duration) %>%
                       mutate(adj_prot_share=prot_share-lag_prot_share) %>%
                       mutate(adj_shelter=adj_prot_share*pot_shelter_time*value_time) %>%
                       mutate(adj_shelter2=adj_prot_share*pot_shelter_time*value_time2) %>%
                       filter(prob>0.05)-> aggr_prob_forecast

aggr_prob_forecast %>% mutate(adj_prot_share=100*adj_prot_share) %>% select(housing, adj_prot_share, value_time, value_time2)->het_costs_table

het_costs_table
print(xtable(het_costs_table, type = "latex"), file = "Output/opp_costs.tex")

aggr_prob_forecast %>% mutate(adj_prot_share=100*adj_prot_share) %>% select(housing, prot_share, adj_prot_share, value_time2)->het_costs_table2

het_costs_table2
print(xtable(het_costs_table2, type = "latex"), file = "Output/opp_costs2.tex")

het_costs_table %>% write.csv('../Output/het_costs_table.csv')

# Assuming that forecasts of different probs (except 0) have the same average annual area:
#-> averaging out sheltering times
#-> weighting injuries and fatalities by probabilities

#Note: Keep tornado strike zones constant across forecasts, but their responses and time sheltering changes

#if assuming the same strike area in each warning probability:
#aggr_prob_forecast%>%group_by(time,prob)%>%summarize(totfat=sum(exp_fatalities))%>%group_by(time)%>%summarize(totfat=mean(totfat))

#WE SHOULD ALSO HAVE <5% predicted probability, because the prob is never exactly zero

#Averaged probabilistic forecast with uniform opportunity costs:
aggr_prob_forecast %>% group_by(time) %>%
  summarise(sum_injuries=sum(exp_injuries), sum_fatalities=sum(exp_fatalities), sum_timecosts=10^(-6)*sum(time_costs), pop_warned_prob=sum(pop_warned_prob)) %>%
  mutate(costs_fatalities=VSL*sum_fatalities, costs_injuries=0.01*VSL*sum_injuries) %>%
  mutate(totalcosts=costs_fatalities+costs_injuries+sum_timecosts) %>%
  mutate(warn_type="Probabilistic") %>%
  select(time, warn_type, sum_fatalities, sum_injuries, costs_fatalities, costs_injuries, sum_timecosts, totalcosts) %>%
  t()%>%as.data.frame() ->prob_estim 

#averaging across 100% deterministic and no warning accounting for POD:
prob_estim<-cbind(nowarning_estim,prob_estim)
names(prob_estim)<-c("nowarn_costs","prob_costs")
prob_estim %>% tibble::rownames_to_column('cost') %>% slice(3:n()) %>% mutate(prob_costs=as.numeric(prob_costs), nowarn_costs=as.numeric(nowarn_costs)) %>%
  mutate(prob_costs=nowarn_costs*(1-POD_prob)+prob_costs*POD_prob)->prob_estim
print(prob_estim)


#Probabilistic forecast with heterogeneous opportunity costs:
aggr_prob_forecast %>% group_by(time) %>%
  summarise(sum_injuries=sum(exp_injuries), sum_fatalities=sum(exp_fatalities), sum_timecosts=10^(-6)*sum(adj_shelter), sum_timecosts2=10^(-6)*sum(adj_shelter2)) %>%
  mutate(costs_fatalities=VSL*sum_fatalities, costs_injuries=0.01*VSL*sum_injuries) %>%
  mutate(totalcosts=costs_fatalities+costs_injuries+sum_timecosts) %>%
  mutate(totalcosts2=costs_fatalities+costs_injuries+sum_timecosts2) %>%
  mutate(warn_type="Probabilistic") %>%
  select(time, warn_type, sum_fatalities, sum_injuries, costs_fatalities, costs_injuries, sum_timecosts, totalcosts, sum_timecosts2, totalcosts2) %>%
  t()%>%as.data.frame() ->het_prob_estim 

het_prob_estim[-c(1,2),] %>% as.numeric %>% as.matrix->f1
c(nowarning_estim[-c(1,2),],nowarning_estim[7:8,]) %>% as.numeric %>% as.matrix->f2
het_prob_estim<-f2*(1-POD_prob)+f1*POD_prob
het_prob_estim<-rbind(t(t(het_prob_estim[c(1,2),])),het_prob_estim)
het_prob_estim

#Deterministic forecasts, but accounting for heterogeneous sheltering costs
#Note: for casualties it doesn't matter who is hiding, as casualty rates and VSL are the same 
#It matters for opportunity costs
aggr_prob_forecast %>% ungroup() %>%
  arrange(housing, time, prob) %>%
  mutate(adj_shelt=pmin(adj_prot_share,shelter-lag_prot_share)) %>%
  mutate(adj_shelt=pmax(0,adj_shelt)) %>%
  mutate(pot_shelter_time=pop_props*Pop_warned*warn_duration) %>%
  mutate(shelt_costs=adj_shelt*pot_shelter_time*value_time)%>%
  mutate(shelt_costs2=adj_shelt*pot_shelter_time*value_time2)%>%
  group_by(time)%>% summarise(shelt_costs=10^(-6)*sum(shelt_costs),shelt_costs2=10^(-6)*sum(shelt_costs2))%>%
  t()%>%as.data.frame()->det_heter

warnings_estim_het<-warnings_estim
warnings_estim_het2<-warnings_estim

warnings_estim_het[5,3]<-as.numeric(det_heter[2,1])
warnings_estim_het2[5,3]<-as.numeric(det_heter[3,1])

warnings_estim_het[6,3]<-as.numeric(warnings_estim_het[3,3])+as.numeric(warnings_estim_het[4,3])+as.numeric(warnings_estim_het[5,3])

print(warnings_estim_het)

warnings_estim_het2[6,3]<-as.numeric(warnings_estim_het2[3,3])+as.numeric(warnings_estim_het2[4,3])+as.numeric(warnings_estim_het2[5,3])

print(warnings_estim_het2)

  


warnings_estim <- cbind(warnings_estim,prob_estim[,3])
names(warnings_estim) <- c("V","No_warning","Deterministic","Probabilistic")
print(warnings_estim)
warnings_estim %>% write.csv('../Output/table_warnings_estim.csv')

warnings_estim_het<-cbind(warnings_estim_het, het_prob_estim[3:8,])
names(warnings_estim_het) <- c("V","No_warning","Deterministic","Probabilistic")
print(warnings_estim_het)
warnings_estim_het %>% write.csv('../Output/table_warnings_estim_het.csv')



warnings_estim_het2<-cbind(warnings_estim_het2, het_prob_estim[3:8,1])
warnings_estim_het2[5:6,4]<-het_prob_estim[9:10,1]
names(warnings_estim_het2) <- c("V","No_warning","Deterministic","Probabilistic")
print(warnings_estim_het2)
warnings_estim_het2 %>% write.csv('../Output/table_warnings_estim_het2.csv')





