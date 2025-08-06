source("R_Scripts/1_master_file_cesdata2.R")
ces %>%
  filter(election==2019) %>%
  count(election, mode)

library(nnet)
table(ces$vote)
factor(ces$vote)
ces$vote<-as_factor(ces$vote)
ces$vote<-factor(ces$vote, levels=c("Conservative", "Liberal", "NDP", "BQ"))
table(ces$vote)
#mod1<-multinom(vote~as_factor(occupation4)*mode, data=subset(ces, election==2019))
mod_2015_phone<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2015&mode=="Phone"))
mod_2019_phone<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2019&mode=="Phone"))
mod_2019_web<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2019&mode=="Web"))
mod_2021_web<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2021))
table(ces15phone$occupation3, useNA = "ifany")
table(ces19phone$occupation3, useNA = "ifany")
lookfor(ces19phone, "weight")
library(modelsummary)
library(gt)
model.list1<-list(mod_2015_phone,
     mod_2019_phone, mod_2019_web, mod_2021_web)
names(model.list1)<-c("2015 Phone Unweighted", "2019 Phone Unweighted ", "2019 Web Unweighted", "2021 Web Unweighted")
modelsummary(model.list1,shape=term~response, stars=T, fmt=2, output="gt") %>%
  tab_style(
    style=cell_fill(color="grey"),
    locations=cells_body(columns=c(6,9))
  ) %>%
  gtsave(., filename="comparing_time.html")

#Now show weighted models

ces19phone$occupation3
ces19web$occupation3
ces19web$cps19_panel

ces19phone$occupation4<-Recode(as_factor(ces19phone$occupation3),"'Skilled'='Working Class' ;
       'Unskilled'='Working Class' ;
       'Professional'='Professional' ; 'Managers'='Manager'; 'Self_employed'='Self-Employed';'Routine_Nonmanual'='Routine Non-manual'",
       levels=c("Working Class", "Routine Non-manual", "Self-Employed", "Professional", "Manager"))
ces19web$occupation4<-Recode(as_factor(ces19web$occupation3),"'Skilled'='Working Class' ;
       'Unskilled'='Working Class' ;
       'Professional'='Professional' ; 'Managers'='Manager'; 'Self_employed'='Self-Employed';'Routine_Nonmanual'='Routine Non-manual'",
                               levels=c("Working Class", "Routine Non-manual", "Self-Employed", "Professional", "Manager"))
ces21$occupation4<-Recode(as_factor(ces21$occupation3),"'Skilled'='Working Class' ;
       'Unskilled'='Working Class' ;
       'Professional'='Professional' ; 'Managers'='Manager'; 'Self_employed'='Self-Employed';'Routine_Nonmanual'='Routine Non-manual'",
                             levels=c("Working Class", "Routine Non-manual", "Self-Employed", "Professional", "Manager"))

# Find weight variables
lookfor(ces19web, "weight")
lookfor(ces19phone, "weight")
#Now fit weighted models
ces19phone$vote<-as_factor(ces19phone$vote)
ces19web$vote<-as_factor(ces19web$vote)
ces21$vote<-as_factor(ces21$vote)
table(ces21$vote)
ces19phone$vote<-factor(ces19phone$vote, levels=c("Conservative", "Liberal", "NDP", "Green", "Bloc", "Other"))
ces19web$vote<-factor(ces19web$vote, levels=c("Conservative", "Liberal", "NDP", "Green", "Bloc", "Other"))
ces21$vote<-factor(ces21$vote, levels=c("Conservative", "Liberal", "NDP", "Green", "Bloc", "Other"))
table(ces21$vote)
lookfor(ces21, "weight")
mod_2019_phone_unwtd<-multinom(as_factor(vote)~as_factor(occupation4), data=ces19phone)
mod_2019_phone_wtd<-multinom(as_factor(vote)~as_factor(occupation4), data=subset(ces19phone,!is.na(weight_PES)), weights=weight_PES)
mod_2019_web_wtd<-multinom(as_factor(vote)~as_factor(occupation4), data=subset(ces19web, !is.na(pes19_weight_general_all)), weights=pes19_weight_general_all)
mod_2021_web_wtd<-multinom(as_factor(vote)~as_factor(occupation4), data=subset(ces21, !is.na(pes21_weight_general_all)), weights=pes21_weight_general_all)
model.list2<-list(mod_2019_phone_unwtd, mod_2019_phone_wtd, mod_2019_web_wtd, mod_2021_web_wtd)
names(model.list2)<-c("2019 Phone Unweighted", "2019 Phone Weighted", "2019 Web weighted", "2021 Web Weighted")
modelsummary(model.list2, stars=T, shape=term~response, fmt=2, output="gt") %>%
  tab_style(
    style=cell_fill(color="grey"),
    locations=cells_body(columns=c(3,5,7))
  ) %>%
  gtsave(., filename="comparing_weighting.html")


prop.table(table(ces19web$occupation4, ces19web$vote, useNA="ifany"),1)
prop.table(table(ces19phone$occupation4, ces19phone$vote, useNA="ifany"),1)
ces19phone$weight_PES
library(srvyr)
ces19phone %>%
  filter(!is.na(weight_PES)) %>%
  filter(!is.na(vote)) %>%
  as_survey_design(., weight=weight_PES) %>%
  group_by(occupation4, vote) %>%
  summarize(survey_mean()) %>%
  print(n=24)
ces19web$pes19_weight_general_restricted
ces19web %>%
  filter(!is.na(pes19_weight_general_restricted)) %>%
  filter(!is.na(vote)) %>%
  as_survey_design(., weight=pes19_weight_general_restricted) %>%
  group_by(occupation4, vote) %>%
  summarize(survey_mean()) %>%
  print(n=24)
prop.table(table(ces19phone$vote, useNA = "ifany"))
prop.table(table(ces19web$vote, useNA="ifany"))
table(ces15phone$occupation)
lookfor(ces19phone$NOC)
table(ces19phone$NOC, useNA = "ifany")
ces19phone$p52
