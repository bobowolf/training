################################################################################
#                 Sample Descriptive                                           #
# Guanghua Wang
# Clemson U
# May 25
################################################################################
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# enrollment information
rm(list = ls())
load("~/data/NLSY/NLSY97/data/output/education/enrollment.Rda")

data <- enroll.stat %>% 
  filter(year == 2013)

n_enroll <- summarise(data,
                      n_enroll = sum(enroll.tag1 == 1, na.rm = T),
                      n_no_enroll = sum(enroll.tag1 == 0, na.rm = T),
                      n_percent_enroll = n_enroll / (n_enroll + n_no_enroll))

# The distribution of seniority
rm(list = ls())
load("~/Google Drive/research/training/data/output/constructed_data/sample3.Rda")

g1 <- ggplot(data, aes(tenure_wks_dli)) + 
  geom_bar() +
  labs( x = "seniority(weeks)", y = "count")

# Training related information
rm(list = ls())
load("~/Google Drive/research/training/data/output/constructed_data/")

