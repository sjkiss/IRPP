source("R_Scripts/1_master_file_cesdata2.R")
ces %>%
  filter(election==2019) %>%
  count(election, mode)

library(nnet)
table(ces$vote)
factor(ces$vote)
ces$vote<-as_factor(ces$vote)
ces$vote<-factor(ces$vote, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green", "Other"))
table(ces$vote)
mod1<-multinom(vote~as_factor(occupation4)*mode, data=subset(ces, election==2019))
mod_2015_phone<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2015&mode=="Phone"))
mod_2019_phone<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2019&mode=="Phone"))
mod_2019_web<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2019&mode=="Web"))
mod_2021_web<-multinom(vote~as_factor(occupation4), data=subset(ces, election==2021))

lookfor(ces19phone, "weight")
library(modelsummary)
modelsummary(list(mod_2015_phone, mod_2019_phone, mod_2019_web, mod_2021_web),shape=term~response, stars=T)
#Now show weighted models

ces19phone$occupation3
ces19web$occupation3

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
ces19phone$vote<-factor(ces19phone$vote, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green", "Other"))
ces19web$vote<-factor(ces19web$vote, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green", "Other"))
ces21$vote<-factor(ces21$vote, levels=c("Conservative", "Liberal", "NDP", "BQ", "Green", "Other"))
table(ces21$vote)
lookfor(ces21, "weight")
mod_2019_phone_wtd<-multinom(as_factor(vote)~as_factor(occupation4), data=subset(ces19phone,!is.na(weight_PES)), weights=weight_PES)
mod_2019_web_wtd<-multinom(as_factor(vote)~as_factor(occupation4), data=subset(ces19web, !is.na(pes19_weight_general_all)), weights=pes19_weight_general_all)
mod_2021_web_wtd<-multinom(as_factor(vote)~as_factor(occupation4), data=subset(ces21, !is.na(pes21_weight_general_all)), weights=pes21_weight_general_all)

modelsummary(list(mod_2019_phone_wtd, mod_2019_web_wtd, mod_2021_web_wtd), stars=T, shape=term~response )
