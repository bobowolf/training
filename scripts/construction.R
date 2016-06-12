################################################################################
#                  constrcution
# Guanghua Wang
# Clemson U
# May 24 2016

# June 12 2016
################################################################################

# clean workspace
rm(list = ls())

# load library
library(dplyr)
library(tidyr)
library(lubridate)

# set working directory
setwd("~/Google Drive/research/training/data/")

# load base data set
load("~/data/NLSY/NLSY97/data/output/dataset_construction/first_job2.Rda")

# only keep cross_sectional data
data <- first_job %>%
  filter(cross_sectional == 1) %>% # 4096

# drop tenure_wks_dli > interval_interview
 filter(tenure_wks_dli <= interval_interview) %>% # 3416

# drop tenure_wks_dli >= 68
  filter(tenure_wks_dli < 68) %>% # 3217

# drop start_date < 1996-01-01
  filter(start_date > ymd("1996-01-01")) # 3216

rm(first_job1)

# merge education
load("~/data/NLSY/NLSY97/data/output/education/edu_level.Rda")
data <- left_join(data, edu_level, by = c("id","year")) # 3216
rm(edu_level)

# merge enrollment
load("~/data/NLSY/NLSY97/data/output/education/enrollment.Rda")
data <- left_join(data, enroll.stat, by = c("id","year")) # 3216
rm(enroll.stat)

data$hourly_pay <- replace(
  data$hourly_pay, data$hourly_pay < 5, 5
) 

data$hourly_pay <- replace(
  data$hourly_pay, data$hourly_pay > 40, 40
) 

# created variables
data <- data %>% 
  mutate(
  # log wage
  log_wage = log(hourly_pay),
  # age when start frist full time main job
  age_start_work = year(start_date) - year(birthday_ym),
  # education level category
  edu_level = ifelse(
    lessthanhighschool == 1 | highschool == 1, "high school or less", ifelse(
      somecollege == 1 | college == 1 | graduatesch == 1, "college or more", NA
    )
  ),
  # gender category
  gender = ifelse(
    female == 1, "female", "male"
  ),
  # race category
  race_ethnicity = ifelse(
    black == 1, "black", ifelse(
      hispanic == 1, "hispanic", ifelse(
        white == 1, "white", "other race"
      )
    )
  )
) 


# merge training
load("~/data/NLSY/NLSY97/data/output/training/training.Rda")
training <- training %>%
  select(-year, -job_id)
training_long <- left_join(data, training,
                           by = c("id", "emp_uid"))  %>% # 3568 
  filter(!is.na(trn_id)) %>% # 737
  filter(!(trn_start > stop_date)) %>% # 257
  select(id, emp_uid, contains("trn")) %>%
  mutate(
    trn_length = trunc(
      time_length(interval(trn_start, trn_stop), "week")
    ) + 1
  ) 
## deal with outers trn_length > 104
training_long$trn_length <- replace(
  training_long$trn_length, training_long$trn_length > 104, 104
)

save(training_long, file = "../output/constructed_data/training_long.Rda")

training_wide <- training_long %>%
  mutate(flag = paste(id, trn_uid, sep = "-")) %>%
  distinct(flag) %>%
  select(-flag) %>%
  # erra
  # id = 76 trn_uid = 1002, 1102 duplicated, drop trn_uid = 1002
  filter(!(id == 76 & trn_uid == 1002)) %>%
  # indicate numbers of training respondents received during the job.
  group_by(id) %>%
  mutate(flag = rank(trn_uid)) %>%
  ungroup() %>%
  # transfer date to string to keep the information
  mutate(trn_start = as.character.Date(trn_start),
         trn_stop = as.character.Date(trn_stop)) %>%
  # spread _1, _2, _3 ...
  gather(key = var, value = value.gen, -id, -flag, -emp_uid) %>%
  unite(var_num, var, flag) %>%
  spread(key = var_num, value = value.gen, fill = NA) %>%
  # recover data type
  mutate_each(funs(as.integer), -id, -contains("start"), -contains("stop")) %>%
  mutate_each(funs(ymd), contains("start"), contains("stop")) %>%
  mutate(
    trn_all = as.numeric(!is.na(trn_id_1)) +
      as.numeric(!is.na(trn_id_2)) +
      as.numeric(!is.na(trn_id_3)) +
      as.numeric(!is.na(trn_id_4)) +
      as.numeric(!is.na(trn_id_3)),
    trn_any = as.numeric(!is.na(trn_all))
  ) %>%
  select(id, emp_uid, everything())

rm(training)

data <- left_join(data, training_wide, 
                  by = c("id", "emp_uid"))

# replace NA in the data
data <- data %>%
  replace_na(
    list(
      trn_any = 0, 
      trn_all = 0,
      trn_type_1 = 0
    )
  )

rm(training_wide, training_long)

save(data, file = "../output/constructed_data/sample.Rda")



