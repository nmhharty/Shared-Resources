#Script to load and clean vaccination data for dashboard
#First authored 2/2/2021 by Nicole Harty
#Last update: 4/15/2022

#4/15/2022: dosage_num column was removed from PatientImmunizations.txt file so need to modify analysis to NOT use this column 

# Creating Population Level Vax Dataframes --------------------------------


#load Dosage Report from CIIS, PatientImmunizations from lpha portal
##Has ZIP code, age at receipt, vaccine name, dose #

library(openxlsx)
library(tidyverse)
# DosageReport <- read.delim("Dosage_Report.txt", sep = "|")
PtIZ <- read.delim("../Shared-Resources/COVID-Data-Files/PatientImmunizations_Routt.txt", sep = "|", header = TRUE#, fileEncoding = "utf16"
                   )
# ClinicIZ <- read.delim("ClinicImmunizations_Routt.txt", sep = "|", header = TRUE#, fileEncoding = "utf16"
# )
colnames(PtIZ)
PtIZ <- PtIZ %>%
  mutate(AgeGroup10yr = cut(age_at_1stvaccination, breaks=c(0,9,19,29,39,49,59,69,79,89,130), 
                            labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90 and over")),
         AgeGroup5yr = cut(age_at_1stvaccination, breaks=c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,130), 
                           labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                                    "60-64","65-69","70-74","75-79","80-84","85-89","90-94","95 and over")))
PtIZ[,c(4, 9:10)] <- lapply(PtIZ[,c(4, 9:10)], as.Date.character, "%m/%d/%Y")

PtIZ %>%
 # filter(age_at_1stvaccination>69) %>%
  summarise(people = n_distinct(patient_id))


PtIZ %>%
  group_by(AgeGroup5yr) %>%
  summarise(N = n_distinct(patient_id)) %>%
  left_join(RouttPopTable %>%
              select(AgeGroup5yr, TOTAL) , by = "AgeGroup5yr") %>%
  mutate(Percent = scales::percent(N/TOTAL))


# PtIZ %>%
#   group_by(clinic_county) %>%
#   summarise(NumPeople = n_distinct(patient_id)) %>%
#   arrange(desc(NumPeople))



# Vaccine Efficacy --------------------------------------------------------

#Need to update Fully Vax to label subsequent doses (>2 as fully vax)

#As of 4/14/2022 dosage_num column removed from PatientImmunizations file so need to identify FullyVax using date of dose and type of dose

#create dose_order column to identify order of vaccines received based upon date, regardless of vaccine type
PtIZ <- PtIZ %>%
  arrange(patient_id, vaccination_date) %>%
  group_by(patient_id) %>%
  mutate(DoseRank = rank(vaccination_date)) 

PtIZ <- PtIZ %>% 
  ungroup() %>%
  mutate(FullyVax = case_when(vaccination_code=="COVID-19 Vector-NR (JSN)" ~ "Yes",
                            #  vaccination_code!="COVID-19 Vector-NR (JSN)"&dosage_num==2 ~ "Yes", #remove this when dosage_num removed
                              vaccination_code!="COVID-19 Vector-NR (JSN)"&DoseRank==2 ~ "Yes",
                              TRUE ~ as.character("No")),
         zip = str_sub(zip_code,1,5)) %>%
  mutate(City = case_when(zip %in% c("80477", "80487", "80488") ~ "Steamboat Springs",
                                   zip=="80428" ~ "North Routt",
                                   zip=="80467" ~ "South Routt",
                                   zip=="80469" ~ "South Routt",
                                   zip=="80479" ~ "South Routt",
                                   zip=="80483" ~ "South Routt",
                                   zip=="81639" ~ "West Routt",
                                   TRUE ~ as.character("Incomplete or Missing")),
         DoseDateWeek = isoweek(vaccination_date), 
         DoseDateWeekStart = floor_date(vaccination_date, "week", week_start = getOption("lubridate.week.start", 1)))

FullyVax <- PtIZ %>%
  filter(FullyVax=="Yes") %>%
  select(patient_id, vaccination_date) %>%
  distinct() %>%
  mutate(FullyVaxDate = vaccination_date+14,
         # FullyVaxDateMonth = month(FullyVaxDate),
         FullyVaxDateAttr = case_when(month(FullyVaxDate)==12 ~ (paste0((year(FullyVaxDate)+1),"-01","-01")),
                                      month(FullyVaxDate)<12 ~ (paste0(year(FullyVaxDate),"-",(month(FullyVaxDate)+1),"-01"))),
         FullyVaxDateAttr = as.Date(FullyVaxDateAttr),
         FullyVaxWeekStart = floor_date(FullyVaxDate, "week", week_start = getOption("lubridate.week.start", 1)),
         FullyVaxWeekAttr = FullyVaxWeekStart+7
         )
# FullyVax %>%
#   mutate(FullyVaxDateAttr = case_when(month(FullyVaxDate)<12 ~ (paste0(year(FullyVaxDate),"-",(month(FullyVaxDate)+1),"-01")))) %>%
#   filter(month(FullyVaxDate)!=12) %>%
#   select(FullyVaxDateAttr) %>%
#   distinct() %>%
#   mutate(isDate = as.Date(FullyVaxDateAttr))


#identify VACCINE BREAKTHROUGH
## NOV 17: 
## no longer using this because CEDRS report from LPHA portal has breakthrough column I was recreating 
##  LPHA portal file uses ALL CIIS not just Routt resident records to determine breakthrough. 
##  # CDPHE PORTAL FILE identifies 564 breakthrough as of 11/16. Routt identified through code = 506
##  
# VaxBreakthroughList <- ConfProbCases %>%
#   select(eventid, first_name, last_name, date_of_birth, AttributionDate) %>%
#   inner_join((PtIZ %>%
#                 select(patient_id, patient_first_name, patient_last_name, patient_dob, vaccination_date, vaccination_code, FullyVax) %>%
#                 filter(FullyVax=="Yes")),
#              by = c("first_name" = "patient_first_name", "last_name" = "patient_last_name", "date_of_birth" = "patient_dob")) %>%
#   filter(AttributionDate>vaccination_date) %>%
#   mutate(DaysVaxMinusTest = as.numeric(difftime(AttributionDate,vaccination_date, units = "days")),
#          AttributionDateMonthStart = as.Date(paste0(year(AttributionDate),"-",month(AttributionDate),"-01"))
#          ) %>%
#   filter(DaysVaxMinusTest>=14)
# 
# #Add vaccination info to cases
# DoseInfoVaxBreakthrough <- VaxBreakthroughList %>%
#   select(-vaccination_date, -vaccination_code) %>%
#   left_join((PtIZ %>%
#                select(patient_first_name, patient_last_name, patient_dob, vaccination_date, dosage_num, vaccination_code, clinic_desc)), 
#             by = c("first_name" = "patient_first_name", "last_name" = "patient_last_name", "date_of_birth" = "patient_dob"))


#publish to Google Sheets
# gs4_create("VaxBreakthroughData",
#            sheets = c("Vax Breakthrough List", "Dose Info"))

#commented out 9/8/2021 because this isn't being used
# gs4_auth(email = "routtcodph@gmail.com")
# write_sheet(VaxBreakthroughList, ss = "https://docs.google.com/spreadsheets/d/1H2VLtEQcEy8rRhKS6uTAsqNveCeKC8QwafcdLwRj1mk/edit#gid=294850612",
#             sheet = "Vax Breakthrough List")
# write_sheet(DoseInfoVaxBreakthrough, ss = "https://docs.google.com/spreadsheets/d/1H2VLtEQcEy8rRhKS6uTAsqNveCeKC8QwafcdLwRj1mk/edit#gid=294850612",
#             sheet = "Dose Info")

#PCV PPV Graph

# TwoWeekDates <- Calendar %>%
#   # select(WeekStart) %>%
#   # distinct() %>%
#   # slice(which(row_number() %% 2 == 1)) %>%
#   mutate(TwoWeekEnd = WeekStart+13,
#          TwoWeekStart = WeekStart)
#   isoweek("2020-02-24")

# FullyVax <- PtIZ %>%
#   filter(FullyVax=="Yes") %>%
#   select(patient_id, vaccination_date) %>%
#   distinct() %>%
#   mutate(FullyVaxDate = vaccination_date+14,
#          FullyVaxDateMonth = month(FullyVaxDate),
#          FullyVaxMonthAttr = case_when(FullyVaxDateMonth==12 ~ 1,
#                                        FullyVaxDateMonth<12 ~ (FullyVaxDateMonth+1)),
#          FullyVaxWeekStart = floor_date(FullyVaxDate, "week", week_start = getOption("lubridate.week.start", 1)),
#          FullyVaxWeekAttr = FullyVaxWeekStart+7)
# FullyVax$FullyVaxMonthAttr <- month(FullyVax$FullyVaxMonthAttr, label = TRUE)


# FullyVax$FullyVaxMonthAttr <- month(FullyVax$FullyVaxMonthAttr, label = TRUE)


# Boosters and Third Doses ------------------------------------------------

#identify booster recipients

#identify J&J, MOD, and PFZ recipients
JJrecipient <- PtIZ %>%
  filter(vaccination_code=="COVID-19 Vector-NR (JSN)"&DoseRank==1)

MODrecipient <- PtIZ %>%
  filter(vaccination_code=="COVID-19 mRNA (MOD)"&DoseRank==1) %>%
  select(patient_id) %>%
  left_join(PtIZ %>%
              filter(vaccination_code=="COVID-19 mRNA (MOD)"&DoseRank==2), by = "patient_id")

PFZrecipient <- PtIZ %>%
  filter((vaccination_code=="COVID 12+yrs PURPLE CAP"|vaccination_code=="COVID 12+yrs GRAY CAP"|
            vaccination_code=="COVID AGE 5-11 ORANGE CAP")&DoseRank==1) %>%
  select(patient_id) %>%
  left_join(PtIZ %>%
              filter((vaccination_code=="COVID 12+yrs PURPLE CAP"|vaccination_code=="COVID 12+yrs GRAY CAP"|
                        vaccination_code=="COVID AGE 5-11 ORANGE CAP")&DoseRank==2), by = "patient_id")

#For each first dose type, identify those who received 1st booster, 2nd booster
#this doesn't account for time between doses, merely looks at DoseRank. previous analysis suggests very low rate of "bad data" or incorrect schedule
JJrecipient <- JJrecipient %>%
  select(patient_id) %>%
  left_join(PtIZ, by = "patient_id") %>%
  mutate(FirstBooster = case_when(DoseRank==2 ~ "TRUE",
                                  TRUE ~ as.character("FALSE")),
         SecondBooster = case_when(DoseRank==3 ~ "TRUE",
                                  TRUE ~ as.character("FALSE")))

MODrecipient <- MODrecipient %>%
  filter(FullyVax=="Yes") %>%
  select(patient_id) %>%
  left_join(PtIZ, by = "patient_id") %>%
  mutate(FirstBooster = case_when(DoseRank==3 ~ "TRUE",
                                  TRUE ~ as.character("FALSE")),
         SecondBooster = case_when(DoseRank==4 ~ "TRUE",
                                   TRUE ~ as.character("FALSE")))

PFZrecipient <- PFZrecipient %>%
  filter(FullyVax=="Yes") %>%
  select(patient_id) %>%
  left_join(PtIZ , by = "patient_id") %>%
  mutate(FirstBooster = case_when(DoseRank==3 ~ "TRUE",
                                  TRUE ~ as.character("FALSE")),
         SecondBooster = case_when(DoseRank==4 ~ "TRUE",
                                   TRUE ~ as.character("FALSE")))

BoosterList <- JJrecipient %>%
  rbind(MODrecipient) %>%
  rbind(PFZrecipient)


# Queries - not run -------------------------------------------------------


# 
# FullyVax %>%
#   group_by(FullyVaxWeekAttr) %>%
#   summarise(Count = n()) %>%
#   mutate(RollingSumFullyVax = cumsum(Count),
#          FullyVaxPercent = round((RollingSumFullyVax/RouttPop)*100,2),
#          NotFullyVaxPercent = round(((RouttPop-RollingSumFullyVax)/RouttPop)*100,2)) %>%
#   left_join(VaxBreakthroughList %>%
#               mutate(AttributionMonth = month(AttributionDate)) %>%
#               #   filter(AttributionMonth<month(Sys.Date(), label = TRUE), AttributionMonth!="Jan") %>%
#               group_by(AttributionMonth) %>%
#               summarise(VaxBreakthrough = n()) %>%
#               left_join(ConfProbCases %>%
#                           mutate(AttributionMonth = month(AttributionDate)) %>%
#                           #   filter(AttributionMonth<month(Sys.Date(), label = TRUE), Age>11, AttributionMonth!="Jan") %>%
#                           group_by(AttributionMonth) %>%
#                           summarise(Total = n()), by = "AttributionMonth") %>%
#               mutate(VaxBreakthroughPercent = round((VaxBreakthrough/Total)*100,2)), by = c("FullyVaxMonthAttr" = "AttributionMonth")) %>%
#     plot_ly(
#     x = ~FullyVaxPercent,
#     y = ~VaxBreakthroughPercent,
#     type = "scatter") %>%
#   layout(margin = list(l=50,r=50,h=100),
#          title = "TITLE",
#          xaxis = list(title ="PPV",
#                       range = c(0,100)),
#          yaxis = list(title = "PCV",
#                       range = c(0,100)),
#          font = list(family = "Arial", size = 12))

# UNDER 18
# PtIZ %>%
#   filter(FullyVax=="Yes", age_at_1stvaccination<18) %>%
#   select(patient_id) %>%
#   distinct() %>%
#   summarise(TotalFullyVax = n()) /
#   (RouttPopTable2 %>%
#      filter(AGE<19) %>%
#      summarise(TotalPop = sum(TOTAL))) 
# 
# MIDDLE SCHOOL AGE
# PtIZ %>%
#   filter(FullyVax=="Yes", age_at_1stvaccination<15, age_at_1stvaccination>10) %>%
#   select(patient_id) %>%
#   distinct() %>%
#   summarise(TotalFullyVax = n()) /
#   (RouttPopTable2 %>%
#      filter(AGE<15, AGE>10) %>%
#      summarise(TotalPop = sum(TOTAL))) 
# 
# HIGH SCHOOL AGE
# PtIZ %>%
#   filter(FullyVax=="Yes", age_at_1stvaccination<18, age_at_1stvaccination>11) %>%
#   select(patient_id) %>%
#   distinct() %>%
#   summarise(TotalFullyVax = n()) /
#   (RouttPopTable2 %>%
#      filter(AGE<18, AGE>11) %>%
#      summarise(TotalPop = sum(TOTAL)))

# #USING DOB TO CALCULATE AGE TODAY - FULLY VAX
# PtIZ %>%
#   mutate(AgeToday = time_length(Sys.Date()-patient_dob, "years")) %>%
#   filter(FullyVax=="Yes", AgeToday<19, AgeToday>11) %>%
#   select(patient_id) %>%
#   distinct() %>%
#   summarise(TotalFullyVax = n()) /
#   (RouttPopTable2 %>%
#      filter(AGE<18, AGE>11) %>%
#      summarise(TotalPop = sum(TOTAL)))

# #USING DOB TO CALCULATE AGE TODAY - ANY DOSE
# PtIZ %>%
#   mutate(AgeToday = time_length(Sys.Date()-patient_dob, "years")) %>%
#   filter(AgeToday<19, AgeToday>11) %>%
#   select(patient_id) %>%
#   distinct() %>%
#   summarise(TotalFullyVax = n()) /
#   (RouttPopTable2 %>%
#      filter(AGE<18, AGE>11) %>%
#      summarise(TotalPop = sum(TOTAL)))
#      
