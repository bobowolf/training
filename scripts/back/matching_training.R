################################################################################
#                Match training                                                #
################################################################################

library(dplyr)
library(tidyr)
library(lubridate)

# set working directory
setwd("~/Google Drive/research/training/data/scripts")

# sample
load("../output/constructed_data/sample1.Rda")
sample <- data
rm(data)

# training
load("~/data/NLSY/NLSY97/data/output/training/training.Rda")

# merge data
data <- left_join(sample, training,
                  by = c("id", "emp_uid")) %>%
  filter(trn_start < stop_date)

# training starting date after job stoping date
n_trn_start_more_job_stop <- 
  sum(data$trn_start < data$stop_date & data$trn_start > data$start_date,
      na.rm = T)





