################################################################################
#                Training    Desriptive                                        #
# Guanghua Wang
# Clemson U
# May 26
################################################################################
rm(list = ls())

library(dplyr)
library(tidyr)
library(lubridate)
library(stargazer)



# Descriptive
setwd("~/Google Drive/research/training/data/")

## training based
load("output/constructed_data/training_long.Rda")

df <- training_long
rm(training_long)

trn_num <- table(df$trn_type)

firm_helped <- sum(df$trn_employer_paid, na.rm = T)

firm_required <- sum(df$trn_employer_required, na.rm = T)

# collapsed training type
df <- df %>%
  mutate(
    trn_type_collapsed = ifelse(
      trn_type == 9 | trn_type == 10 | trn_type ==11, "company_training", ifelse(
        trn_type== 3, "apprenticeship", "out_of_firm_training"
      )
    )
  )

write.csv(table(df$trn_other_reason, df$trn_type_collapsed),
          file = "output/statistics/training_reason.csv")
df %>% 
  group_by(trn_type_collapsed) %>%
  summarise(length = mean(trn_length, na.rm = T))

################################################################################
#              Individual Level
################################################################################
rm(list = ls())
setwd("~/Google Drive/research/training/data/")
load("output/constructed_data/sample.Rda")

trn_individual <- sum(data$trn_any, na.rm = T)

table(data$trn_all)

## charateristics of workers who receive training
data <- data %>%
  filter(trn_any == 1)

data <- group_by(data, female)
table1 <- data %>%
  summarise(
    observation = n(), 
    mean_age_worked = mean(year(start_date) - year(birthday_ym)),
    median_age_worked = median(year(start_date) - year(birthday_ym)),
    percent_white = sum(white, na.rm =T) / observation * 100,
    percent_black = sum(black, na.rm =T) / observation * 100,
    percent_hispanic = sum(hispanic, na.rm =T) / observation * 100,
    mean_tenure = mean(tenure_job, na.rm = T),
    median_tenure = median(tenure_job,na.rm =T),
    mean_hours = mean(hours, na.rm = T),
    mean_grades = mean(edu, na.rm = T),
    median_grades = median(edu, na.rm = T),
    graduate_school = sum(graduatesch, na.rm = T)/ observation * 100,
    college = sum(college, na.rm = T)/ observation * 100,
    some_college = sum(somecollege, na.rm = T)/ observation * 100,
    high_school = sum(highschool, na.rm =T) / observation * 100,
    high_school_less = sum(lessthanhighschool, na.rm = T) / observation * 100
  )

data <- ungroup(data)
table2 <- data %>%
  summarise(
    observation = n(), 
    mean_age_worked = mean(year(start_date) - year(birthday_ym)),
    median_age_worked = median(year(start_date) - year(birthday_ym)),
    percent_white = sum(white, na.rm =T) / observation * 100,
    percent_black = sum(black, na.rm =T) / observation * 100,
    percent_hispanic = sum(hispanic, na.rm =T) / observation * 100,
    mean_tenure = mean(tenure_job, na.rm = T),
    median_tenure = median(tenure_job,na.rm =T),
    mean_hours = mean(hours, na.rm = T),
    mean_grades = mean(edu, na.rm = T),
    median_grades = median(edu, na.rm = T),
    graduate_school = sum(graduatesch, na.rm = T)/ observation * 100,
    college = sum(college, na.rm = T)/ observation * 100,
    some_college = sum(somecollege, na.rm = T)/ observation * 100,
    high_school = sum(highschool, na.rm =T) / observation * 100,
    high_school_less = sum(lessthanhighschool, na.rm = T) / observation * 100
  )
table <- rbind(table2,table1[,-1])

table <- t(table)

colnames(table) <- c("Full sample", "Male", "Female")

stargazer(table, summary = F, type = "html",
          out = "output/statistics/trn_sample_statistics.html")
stargazer(table, summary = F, type = "latex",
          out = "output/statistics/trn_sample_statistics")

write.csv(table, file = "output/statistics/trn_sample_statistics.csv")

################################################################################
#                   regression                                                 #
################################################################################
rm(list = ls())
setwd("~/Google Drive/research/training/data/")
load("output/constructed_data/sample.Rda")

reg_df1 <- as.data.frame(
  data %>% mutate(
    year_f = factor(year)) %>%
    select( log_wage,
            trn_any,
            tenure_wks_dli,
            year_f,
            # age,
            female,
            edu,
            white, black, hispanic, 
            contains("occ"), - occ_code, - occ_executive,
            # no observations in these occupations
            - occ_health_practicioner, -occ_health_tech,  
            -occ_protective, -occ_funeral, -occ_military,
            contains("ind"), - ind_code, -ind_manufacturing,
            # no observations in these industries
            -ind_entertainment,-ind_military
    )
)

reg1 <- lm(log_wage ~ ., reg_df1)
summary(reg1)

# three types of training

data <- data %>%
  replace_na(list(trn_type_1 = 0))
reg_df2 <- as.data.frame(
  data %>% mutate(
    trn_company = as.numeric(trn_type_1 == 9 | trn_type_1 == 10 | trn_type_1 ==11),
    trn_apprentice = as.numeric(trn_type_1 == 3),
    trn_out_of_firm = as.numeric(trn_type_1 == 1 |
                                   trn_type_1 == 2 | 
                                   (trn_type_1 >= 4  & trn_type_1 <= 8) |
                                   trn_type_1 >=  12),
    year_f = factor(year)
 ) %>%
    select( log_wage,
            trn_company,
            trn_apprentice,
            trn_out_of_firm,
            tenure_wks_dli,
            year_f,
            # age,
            female,
            edu,
            white, black, hispanic, 
            contains("occ"), - occ_code, - occ_executive,
            # no observations in these occupations
            - occ_health_practicioner, -occ_health_tech,  
            -occ_protective, -occ_funeral, -occ_military,
            contains("ind"), - ind_code, -ind_manufacturing,
            # no observations in these industries
            -ind_entertainment,-ind_military
    )
)

reg2 <- lm(log_wage ~ ., reg_df2)
summary(reg2)

## add control whether training was required by firms
data <- data %>%
  replace_na(list(trn_type_1 = 0, trn_employer_required_1 = 0))
reg_df3 <- as.data.frame(
  data %>% mutate(
    trn_company = as.numeric(trn_type_1 == 9 | trn_type_1 == 10 | trn_type_1 ==11),
    trn_apprentice = as.numeric(trn_type_1 == 3),
    trn_out_of_firm = as.numeric(trn_type_1 == 1 |
                                   trn_type_1 == 2 | 
                                   (trn_type_1 >= 4  & trn_type_1 <= 8) |
                                   trn_type_1 >=  12),
    trn_required = ifelse(trn_employer_required_1 == 1, 1, 0),
    year_f = factor(year)
  ) %>%
    select( log_wage,
            trn_company,
            trn_apprentice,
            trn_out_of_firm,
            tenure_wks_dli,
            year_f,
            # age,
            female,
            edu,
            white, black, hispanic, 
            contains("occ"), - occ_code, - occ_executive,
            # no observations in these occupations
            - occ_health_practicioner, -occ_health_tech,  
            -occ_protective, -occ_funeral, -occ_military,
            contains("ind"), - ind_code, -ind_manufacturing,
            # no observations in these industries
            -ind_entertainment,-ind_military,
            trn_required
    )
)

reg3 <- lm(log_wage ~ ., reg_df3)
summary(reg3)

stargazer(reg1, reg2, reg3, type = "html", style = "aer", 
          omit = c("^occ","^ind","^year"),
          out = "output/regression/wage_regression.html")

stargazer(reg1, reg2, reg3, type = "latex", style = "aer", 
          omit = c("^occ","^ind","^year"),
          out = "output/regression/wage_regression")

