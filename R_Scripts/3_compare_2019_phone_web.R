source("R_Scripts/1_master_file_cesdata2.R")
ces %>% 
  filter(election==2019) %>% 
  count(election, mode)

library(nnet)
table(ces$occupation4, ces$election)
mod1<-multinom(vote3~as_factor(occupation4)*election, data=subset(ces, election==2019))
library(modelsummary)
modelsummary(mod1, shape=term~response)
