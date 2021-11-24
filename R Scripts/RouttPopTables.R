#Script to create Population table
#First authored 10/15/2021 by Nicole Harty
#Last update: 10/15/2021



# Population Tables -------------------------------------------------------

#Routt Pop by Geography, 2019 data
RouttPopGeog <- data.frame(City=c("North Routt", "South Routt", "West Routt", "Steamboat Springs"),
                           Pop=c(706,3194,2463,17212))
RouttPopGeog2020 <- data.frame(City=c("North Routt", "South Routt", "West Routt", "Steamboat Springs"),
                           Pop=c(821,3323,2610,18075))

RouttPopTable <- read.csv("../Shared-Resources/COVID-Data-Files/county_sya_data.csv", sep = "\t")
RouttPopTable <- RouttPopTable %>%
  mutate(AgeGroup5yr = str_replace(AGE," to ","-"),
         AgeGroup10yr = c("0-9", "0-9", "10-19", "10-19", "20-29", "20-29", "30-39", "30-39","40-49", "40-49","50-59", "50-59", 
                          "60-69", "60-69","70-79", "70-79","80-89", "80-89","90 and over", "90 and over"))

RouttPopTable2 <- read.csv("../Shared-Resources/COVID-Data-Files/county_sya_data_allyears.csv")
RouttPopTable2 <- RouttPopTable2 %>%
  mutate(AgeGroup5yr = case_when(AGE<5 ~ "0-4",
                                 AGE<10 ~ "5-9",
                                 AGE<15 ~ "10-14",
                                 AGE<20 ~ "15-19",
                                 AGE<25 ~ "20-24",
                                 AGE<30 ~ "25-29",
                                 AGE<35 ~ "30-34",
                                 AGE<40 ~ "35-39",
                                 AGE<45 ~ "40-44",
                                 AGE<50 ~ "45-49",
                                 AGE<55 ~ "50-54",
                                 AGE<60 ~ "55-59",
                                 AGE<65 ~ "60-64",
                                 AGE<70 ~ "65-69",
                                 AGE<75 ~ "70-74",
                                 AGE<80 ~ "75-79",
                                 AGE<85 ~ "80-84",
                                 AGE<90 ~ "85-89",
                                 AGE<95 ~ "90-94",
                                 AGE<100 ~ "95-99",
                                 AGE>=100 ~ "100+"
  ),
  AgeGroup10yr = case_when(AgeGroup5yr=="0-4" ~ "0-9",
                           AgeGroup5yr=="5-9" ~ "0-9",
                           AgeGroup5yr=="10-14" ~ "10-19",
                           AgeGroup5yr=="15-19" ~ "10-19",
                           AgeGroup5yr=="20-24" ~ "20-29",
                           AgeGroup5yr=="25-29" ~ "20-29",
                           AgeGroup5yr=="30-34" ~ "30-39",
                           AgeGroup5yr=="35-39" ~ "30-39",
                           AgeGroup5yr=="40-44" ~ "40-49",
                           AgeGroup5yr=="45-49" ~ "40-49",
                           AgeGroup5yr=="50-54" ~ "50-59",
                           AgeGroup5yr=="55-59" ~ "50-59",
                           AgeGroup5yr=="60-64" ~ "60-69",
                           AgeGroup5yr=="65-69" ~ "60-69",
                           AgeGroup5yr=="70-74" ~ "70-79",
                           AgeGroup5yr=="75-79" ~ "70-79",
                           AgeGroup5yr=="80-84" ~ "80-89",
                           AgeGroup5yr=="85-89" ~ "80-89",
                           AgeGroup5yr=="90-94" ~ "90-99",
                           AgeGroup5yr=="95-99" ~ "90-99",
                           AgeGroup5yr=="100+" ~ "100+"))

RouttPopTable2[,c(6,7)] <- lapply(RouttPopTable2[,c(6,7)], as.factor)