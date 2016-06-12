################################################################################
#           Starting Wage and training                                                     
################################################################################
rm(list = ls())

library(dplyr)

setwd("~/Google Drive/research/training/data/scripts")

load("../output/constructed_data/starting_wage.Rda")

reg_df <- as.data.frame(
  data_predict %>% mutate(# tenure_square = tenure_job**2,
                          trn_type_f = factor(trn_type_1),
                          #trn_type_f = as.numeric(trn_type_1 > 0),
                          year_f = factor(year)) %>%
    select(log_starting_wage, 
           #log_wage = log_wage,
           trn_type_f,
           # tenure_job, tenure_square,
           # year_f,
           female,
           contains("occ"), - occ_code, - occ_executive,
           # no observations in these occupations
           - occ_health_practicioner, -occ_health_tech,  
           -occ_protective, -occ_funeral, -occ_military,
           contains("ind"), - ind_code, -ind_manufacturing,
           # no observations in these industries
           -ind_entertainment,-ind_military,
           highschool, somecollege, college, graduatesch)
)

# reg_df$trn_dum <- replace(reg_df$trn_dum, is.na(reg_df$trn_dum), 0)

summary(lm(log_wage ~ ., reg_df))
