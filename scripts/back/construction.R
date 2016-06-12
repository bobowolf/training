# clean workspace
rm(list = ls())

# load library
library(dplyr)
library(tidyr)
library(lubridate)

# set working directory
setwd("~/Google Drive/research/training/data/scripts")

# load base data set
load("~/data/NLSY/NLSY97/data/output/dataset_construction/first_job.Rda")

# merge start_stop_date
load("~/data/NLSY/NLSY97/data/output/job/job_history/start_stop_date.Rda")
data <- left_join(first_job, start_stop_date, 
                  by = c("id", "year", "job_id"))
rm(first_job, start_stop_date)

## replace starting date earlier than 1997 with NA
data$start_date <- replace(
  data$start_date, data$start_date < ymd(19970101), NA
)

## generate job tenure
data <- data %>%
  mutate(
    tenure_job = trunc(
      time_length(interval(start_date, stop_date), unit = "week")
    ) + 1
  )

# merge time_invariant variable
load("~/data/NLSY/NLSY97/data/output/time_invariant/time_invariant.Rda")
data <- left_join(data, time_invariant, by = "id")
rm(time_invariant)

# merge education level 
load("~/data/NLSY/NLSY97/data/output/education/edu_level.Rda")
data <- left_join(data, edu_level, by = c("id", "year"))
rm(edu_level)

# merge interview date
load("~/data/NLSY/NLSY97/data/output/xwalk/interview_date.Rda")
data <- left_join(data, interview_date, by = c("id", "year")) %>%
  select(-cnt_month)
rm(interview_date)

# tidy it
data <- data %>% 
  select(
    id:year, 
    female:birthday_cnt_month, 
    edu:graduatesch, everything()
)

save(data, file = "../output/constructed_data/sample1.Rda")

################################################################################
#     Tenure DLI > 52 weeks
################################################################################

# generate tenure since the date of last interview
data <- data %>%
  mutate(
    tenure_wks_dli_gen = trunc(
      time_length(interval(start_date, interview_date), "week") 
    ) + 1,
    tenure_wks_dli_less = pmin(tenure_wks_dli, tenure_wks_dli_gen, na.rm = T)
  )

tenure_52wks_more = sum(data$tenure_wks_dli > 52, na.rm = T) # 1907

# Even if I calcuate the tenure by hand, and choose the smallest one, there
# are 1813 obs. whose tenures are bigger than 52.

# drop tenure_wks_dli > 52
data <- data %>%
  filter(tenure_wks_dli <= 52) %>%
  select(-tenure_wks_dli_gen, -tenure_wks_dli_less)

save(data, file = "../output/constructed_data/sample2.Rda")


