#Script to review priority address data
#First authored 2/28/2022 by Nicole Harty
#Last update: 2/28/2022
#
#source data downloaded from LPHA portal

library(tidyverse)
library(lubridate)
library(openxlsx)


PriorityAddress <- read.delim("../Shared-Resources/COVID-Data-Files/elr_priority_address_Routt.txt", sep = "|")


colnames(PriorityAddress)

PriorityAddress %>%
  filter(priority=="Y") %>%
  group_by(facilityName) %>%
  count()