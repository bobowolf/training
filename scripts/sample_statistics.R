library(dplyr)
library(lubridate)
library(stargazer)
library(ggplot2)

rm(list = ls())

setwd("~/Google Drive/research/training/data/")

load("output/constructed_data/sample.Rda")

# sample statistics

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
          out = "output/statistics/sample_statistics.html")
stargazer(table, summary = F, type = "latex",
          out = "output/statistics/sample_statistics")

write.csv(table, file = "output/statistics/sample_statistics.csv")

# other statistics
## age distriubtion by gender
hist(data$age_start_work)

age_gender <- data %>%
  group_by(female) %>%
  summarise(age = mean(age_start_work, na.rm = T))

age_race <- data %>%
  group_by(race_ethnicity) %>%
  summarise(age = mean(age_start_work, na.rm = T))

age_edu <- data %>%
  group_by(edu_level) %>%
  summarise(age = mean(age_start_work, na.rm = T))

# ggplot(data, aes(x = year(start_date) - year(birthday_ym))) + 
#   geom_bar() + facet_grid(female ~ .)

## tenure 
tenure_gender <- data %>%
  group_by(female) %>%
  summarise(tenure = mean(tenure_job, na.rm = T))

tenure_race <- data %>%
  group_by(race_ethnicity) %>%
  summarise(tenure = mean(tenure_job, na.rm = T))

tenure_edu <- data %>%
  group_by(edu_level) %>%
  summarise(tenure = mean(tenure_job, na.rm = T))

### tenure distribution by gender
ggplot(data, aes(x = tenure_job)) + 
  geom_bar() + 
  facet_grid(female ~ .) + 
  xlim(c(10,100))


### tenure distribution by  education
# df_tenure <- data %>%
#   select(tenure_job,
#          female,
#          black, hispanic, white, other_race,
#          edu, degree,
#          lessthanhighschool, highschool, somecollege, college, graduatesch) %>%
#   mutate(edu_level = ifelse(
#     lessthanhighschool ==1, "less than high school", ifelse(
#       highschool == 1, "high school", ifelse(
#         somecollege == 1, "some college", ifelse(
#           college == 1, "college", ifelse(
#             graduatesch == 1, " graduate school", NA
#           )
#         )
#       )
#     )
#   ),
#   gender = ifelse(
#     female == 1, "female", "male"
#   ),
#   race_ethnicity = ifelse(
#     black == 1, "black", ifelse(
#       hispanic == 1, "hispanic", ifelse(
#         white == 1, "white", "other race"
#       )
#     )
#   )
#   )

df_tenure_collapse <- data %>%
  select(tenure_job, 
         gender,
         race_ethnicity,
         edu_level) %>%
 filter(!is.na(edu_level))
  
#setEPS()
#postscript("output/statistics/job_tenure_gender_edu.eps" )
png("output/statistics/job_tenure_gender_edu.png")
ggplot(df_tenure_collapse, aes(x = tenure_job)) + 
  stat_density() + 
  facet_grid(edu_level ~ gender, drop = T) + 
  xlim( c(10,100)) + 
  labs(x = "job tenure")
dev.off()

png("output/statistics/job_tenure_gender_race.png")
ggplot(df_tenure_collapse, aes(x = tenure_job)) + 
  stat_density() + 
  facet_grid(race_ethnicity ~ gender, drop = T) + 
  xlim( c(10,100)) + 
  labs(x = "job tenure")
dev.off()

# working hours
df_hours_collapse <- data %>%
  select( hours,
    gender,
    race_ethnicity,
    edu_level)

hours_full <- data %>%
  summarise(hours = mean(hours, na.rm = T))

hours_gender <- data %>%
  group_by(female) %>%
  summarise(hours = mean(hours, na.rm = T))

hours_race <- data %>%
  group_by(race_ethnicity) %>%
  summarise(hours = mean(hours, na.rm = T))

hours_edu <- data %>%
  group_by(edu_level) %>%
  summarise(hours = mean(hours, na.rm = T))



png("output/statistics/hours_gender_edu.png")
ggplot(df_hours_collapse, aes(x = hours)) + 
  stat_density() + 
  facet_grid(edu_level ~ gender, drop = T) + 
  xlim( c(30,60)) + 
  labs(x = "working hours")
dev.off()

png("output/statistics/hours_gender_race.png")
ggplot(df_hours_collapse, aes(x = hours)) + 
  stat_density() + 
  facet_grid(race_ethnicity ~ gender, drop = T) + 
  xlim( c(30,60)) + 
  labs(x = "working hours")
dev.off()

# seniority
df_seniority_collapse <- data %>%
  select(seniority= tenure_wks_dli, 
         gender,
         race_ethnicity,
         edu_level
  ) %>% 
 filter(!is.na(edu_level))

png("output/statistics/seniority_full_sample.png")
ggplot(df_seniority_collapse, aes(x = seniority)) + 
  geom_bar() + 
  labs(x = "seniority")
dev.off()

# mean and media
summary(data$tenure_wks_dli)

seniority_gender <- data %>% 
  group_by(female) %>%
  summarise(mean(tenure_wks_dli, na.rm = T))

seniority_race <- data %>% 
   group_by(race_ethnicity) %>%
  summarise(mean(tenure_wks_dli, na.rm = T))

seniority_edu <- data %>%
  group_by(edu_level) %>%
  summarise(mean(tenure_wks_dli, na.rm = T))

png("output/statistics/seniority_race.png")
ggplot(data, aes(x = tenure_wks_dli)) + 
  geom_bar() + 
  facet_grid(race_ethnicity ~ .) + 
  xlim( c(30,60)) + 
  labs(x = "seniority")
dev.off()

png("output/statistics/seniority_edu.png")
tmp <- data %>%
  filter(!is.na(edu_level))
ggplot(tmp, aes(x = tenure_wks_dli)) + 
  geom_bar() + 
  facet_grid(edu_level ~ .) + 
  xlim( c(30,60)) + 
  labs(x = "seniority")
dev.off()