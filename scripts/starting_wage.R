################################################################################
#           Starting Wage                                                      
################################################################################
rm(list = ls())

library(dplyr)
library(lubridate)
library(stargazer)
library(ggplot2)

setwd("~/Google Drive/research/training/data/scripts")

load("../output/constructed_data/sample.Rda")

# replace hourly_pay =< 5 or >= 40 with 5 and 40
# 130 obs. < 5
# 40 obs. > 27
data$hourly_pay <- replace(
  data$hourly_pay, data$hourly_pay < 5, 5
) 

data$hourly_pay <- replace(
  data$hourly_pay, data$hourly_pay > 40, 40
) 

# ##############################################################################
# current wage statistics


data <- data %>% mutate(edu_level = ifelse(
      lessthanhighschool == 1 | highschool == 1, "high school or less", ifelse(
        somecollege == 1 | college == 1 | graduatesch == 1, "college or more", NA
      )
    ),
    gender = ifelse(
      female == 1, "female", "male"
    ),
    race_ethnicity = ifelse(
      black == 1, "black", ifelse(
        hispanic == 1, "hispanic", ifelse(
          white == 1, "white", "other race"
        )
      )
    )
  )

wage_full <- data %>%
  summarise(mean = mean(hourly_pay, na.rm = T),
                       median = median(hourly_pay, na.rm =T))
wage_full <- cbind("full",wage_full)
colnames(wage_full) <- c("statistic", "Mean", "Median")
  
wage_race <- data %>%
  group_by(race_ethnicity) %>%
  summarise(mean = mean(hourly_pay, na.rm = T),
            median = median(hourly_pay, na.rm =T))
colnames(wage_race) <- c("statistic", "Mean", "Median")

wage_gender <- data %>%
  group_by(gender) %>%
  summarise(mean = mean(hourly_pay, na.rm = T),
            median = median(hourly_pay, na.rm =T))
colnames(wage_gender) <- c("statistic", "Mean", "Median")

wage_edu <- data %>%
  group_by(edu_level) %>%
  summarise(mean = mean(hourly_pay, na.rm = T),
            median = median(hourly_pay, na.rm =T))
colnames(wage_edu) <- c("statistic", "Mean", "Median")

wage_stat <- rbind(wage_full, wage_gender, wage_race, wage_edu)

wage_stat <- t(wage_stat)

stargazer(wage_stat, summary = F, type = "html",
          out = "../output/statistics/wage_des.html")
stargazer(wage_stat, summary = F, type = "latex",
          out = "../output/statistics/wage_des")

# #############################################################################

data <- data %>% 
  mutate(
    log_wage = log(hourly_pay),
    age = year - year(birthday_ym) 
  ) %>%
  rename(wage = hourly_pay)

df_reg1 <- as.data.frame(
  data %>% mutate(#tenure_dli_square = tenure_wks_dli**2,
                  #tenure_dli_cube = tenure_wks_dli**3,
                  #tenure_dli_fourth = tenure_wks_dli**4,
                  year_f = factor(year)) %>%
    select(log_wage,
         tenure_wks_dli, 
         # tenure_dli_square,
         # tenure_dli_cube,
         # tenure_dli_fourth,
         # age,
         female,
         edu,
         white, black, hispanic, 
         # year_f,
         contains("occ"), - occ_code, - occ_executive,
         # no observations in these occupations
         - occ_health_practicioner, -occ_health_tech,
         -occ_protective, -occ_funeral, -occ_military,
         contains("ind"), - ind_code, -ind_manufacturing,
         # no observations in these industries
         -ind_entertainment,-ind_military
         )
)

reg1 <- lm( log_wage ~ ., df_reg1)
summary(reg1)

stargazer(reg1, type = "html", style = "aer", 
          keep = c("tenure_wks_dli","^age","female",
                   "white","black","hispanic",
                   "highschool", "college","graducatesch",
                   "Constant"),
          out = "../output/regression/wage_regression.html")

stargazer(reg1, type = "latex", style = "aer", 
          keep = c("tenure_wks_dli","^age","female",
                   "white","black","hispanic",
                   "highschool", "college","graducatesch",
                   "Constant"),
          out = "../output/regression/wage_regression")

df_reg1_new <- df_reg1 %>%
  mutate(#tenure_dli_square = 0,
         #tenure_dli_cube = 0,
         #tenure_dli_fourth = 0,
         tenure_wks_dli = 0)

log_starting_wage <- predict(reg1, df_reg1_new)

starting_wage <- exp(log_starting_wage)

data_predict <- cbind(log_starting_wage, starting_wage, data) %>%
  select(id, year, job_id, emp_uid, contains("wage"), everything())

save(data_predict, file = "../output/constructed_data/starting_wage.Rda")


###############################################################################

# starting wage histgram
png("../output/statistics/starting_wage.png")
ggplot(data_predict, aes(x = starting_wage)) + 
  geom_histogram() + 
  labs( x = "starting wage")
dev.off()

# plot the relationship between wage and strating wage
ggplot(data_predict, aes(x = log_wage, y = log_starting_wage)) + 
  geom_point() + 
  labs( x = "log current wage", y = "log starting wage")

# summary statistics 
df_wage <- data_predict %>% select(wage, starting_wage)

stargazer(df_wage, type = "html", 
          out = "../output/statistics/wage_summary.html",
          iqr = T)

## starting_wage summary
starting_wage_full <- data_predict %>%
  summarise(mean = mean(starting_wage, na.rm = T),
            median = median(starting_wage, na.rm =T))
starting_wage_full <- cbind("full",starting_wage_full)
colnames(starting_wage_full) <- c("statistic", "Mean", "Median")

starting_wage_race <- data_predict %>%
  group_by(race_ethnicity) %>%
  summarise(mean = mean(starting_wage, na.rm = T),
            median = median(starting_wage, na.rm =T))
colnames(starting_wage_race) <- c("statistic", "Mean", "Median")

starting_wage_gender <- data_predict %>%
  group_by(gender) %>%
  summarise(mean = mean(starting_wage, na.rm = T),
            median = median(starting_wage, na.rm =T))
colnames(starting_wage_gender) <- c("statistic", "Mean", "Median")

starting_wage_edu <- data_predict %>%
  group_by(edu_level) %>%
  summarise(mean = mean(starting_wage, na.rm = T),
            median = median(starting_wage, na.rm =T))
colnames(starting_wage_edu) <- c("statistic", "Mean", "Median")

starting_wage_stat <- rbind(starting_wage_full, starting_wage_gender, 
                            starting_wage_race, starting_wage_edu)

wage_stat <- t(starting_wage_stat)

stargazer(wage_stat, summary = F, type = "html",
          out = "../output/statistics/starting_wage_des.html")
stargazer(wage_stat, summary = F, type = "latex",
          out = "../output/statistics/starting_wage_des")


