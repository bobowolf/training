# sample statistics 

rm(list = ls())

setwd("~/Google Drive/research/training/data/")

load("output/constructed_data/data1.Rda")

data <- group_by(data, female)
table1 <- data %>%
  summarise(
    observation = n(), 
    mean_age_graduate = mean(year - year(birthday_ym)),
    percent_white = sum(white, na.rm =T) / observation,
    percent_black = sum(black, na.rm =T) / observation,
    percent_hispanic = sum(hispanic, na.rm =T) / observation,
    mean_tenure = mean(tenure_job, na.rm = T),
    median_tenure = median(tenure_job,na.rm =T),
    mean_hours = mean(hours, na.rm = T),
    mean_grades = mean(edu, na.rm = T),
    median_grades = median(edu, na.rm = T),
    high_school_less = sum(edu <= 12, na.rm =T) / observation
    )
data <- ungroup(data)
table2 <- data %>%
  summarise(
    observation = n(), 
    mean_age_graduate = mean(year - year(birthday_ym)),
    percent_white = sum(white, na.rm =T) / observation,
    percent_black = sum(black, na.rm =T) / observation,
    percent_hispanic = sum(hispanic, na.rm =T) / observation,
    mean_tenure = mean(tenure_job, na.rm = T),
    median_tenure = median(tenure_job,na.rm =T),
    mean_hours = mean(hours, na.rm = T),
    mean_grades = mean(edu, na.rm = T),
    median_grades = median(edu, na.rm = T),
    high_school_less = sum(edu <= 12, na.rm =T) / observation
  )

table <- rbind(table2,table1[,-1])

write.csv(table, file = "output/statistics/sample1.csv")

################################################################################

load("output/constructed_data/data2.Rda")

data <- group_by(data, female)
table1 <- data %>%
  summarise(
    observation = n(), 
    mean_age_graduate = mean(year - year(birthday_ym)),
    percent_white = sum(white, na.rm =T) / observation * 100 ,
    percent_black = sum(black, na.rm =T) / observation * 100,
    percent_hispanic = sum(hispanic, na.rm =T) / observation * 100,
    mean_tenure = mean(tenure_job, na.rm = T),
    median_tenure = median(tenure_job,na.rm =T),
    mean_hours = mean(hours, na.rm = T),
    mean_grades = mean(edu, na.rm = T),
    median_grades = median(edu, na.rm = T),
    high_school_less = sum(edu <= 12, na.rm =T) / observation * 100
  )
data <- ungroup(data)
table2 <- data %>%
  summarise(
    observation = n(), 
    mean_age_graduate = mean(year - year(birthday_ym)),
    percent_white = sum(white, na.rm =T) / observation,
    percent_black = sum(black, na.rm =T) / observation,
    percent_hispanic = sum(hispanic, na.rm =T) / observation,
    mean_tenure = mean(tenure_job, na.rm = T),
    median_tenure = median(tenure_job,na.rm =T),
    mean_hours = mean(hours, na.rm = T),
    mean_grades = mean(edu, na.rm = T),
    median_grades = median(edu, na.rm = T),
    high_school_less = sum(edu <= 12, na.rm =T) / observation
  )

table <- rbind(table2,table1[,-1])
write.csv(table, file = "output/statistics/sample2.csv")
