#Script to load and clean all test results and cases data
#First authored 9/10/2020 by Nicole Harty
#Last update: 10/12/2021
#
##CHANGE on 10/12/2021 to source from .txt files in LPHA portal
##LPHA portal files are updated ~4p each day with data through prior day

library(tidyverse)
library(lubridate)
library(openxlsx)


# TestsRawCSV <- read.csv("COVID-Data-Files/CEDRS_COVIDELRTests.csv", skip = 3)
# CasesRawXLSX <- read.xlsx("../Shared-Resources/COVID-Data-Files/CEDRS_NovelCoronavirusListing.xlsx", startRow = 4)

TestsRaw <- read.delim("../Shared-Resources/COVID-Data-Files/elr_tests_Routt.txt", sep = "|")
CasesRaw <- read.delim("../Shared-Resources/COVID-Data-Files/cedrs_Routt.txt", sep = "|")

#UNCOMMENT BELOW LINES WHEN TROUBLESHOOTING. KEEP COMMENTED WHEN RUNNING PRODUCTION - PARAMETER IS SET IN MARKDOWN
# DataThroughDate <- as.Date.character("2021-10-16")
# 
# RecentWeek <- interval(ymd(MostRecentWeekStart), ymd(MostRecentWeekStart+6))
# Recent2Week <- interval(ymd(MostRecentWeekStart-7), ymd(MostRecentWeekStart+6))
# RollingRecentWeek <- interval((DataThroughDate-6), DataThroughDate)
# RollingRecent2Week <- interval((DataThroughDate-13), DataThroughDate)
# RollingRecent2WeekComparison <- interval((DataThroughDate-20), (DataThroughDate-7))
# RollingRecent1WeekComparison <- interval((DataThroughDate-13), (DataThroughDate-7))
# RollingRecent4Week <- interval((DataThroughDate-27), DataThroughDate)



# Clean Up Tests -----------------------------------------------
#colnames(TestsRaw)
TestsRaw[,c(1:3,9,11:14,20:22)] <- lapply(TestsRaw[,c(1:3,9,11:14,20:22)], as.factor)
TestsRaw[,c(7,10,15:17)] <- lapply(TestsRaw[,c(7,10,15:17)], as.Date.character, "%Y-%m-%d")

#Add Routt antigen and SEC tests to TestsRaw

AllTests <- TestsRaw %>%
  bind_rows(readRDS("../Shared-Resources/COVID-Data-Files/StaticTestLogs.RDS")) 

#Add Week and date of week start for collection dates, age at test collection, age grouping
#create collapsed groups of TestType factors and gender
##add person_id info for those added from static test logs
AllTests <- AllTests %>%
  mutate(CollectionDateWeek = isoweek(collectiondate), 
         CollectionWeekStart = floor_date(collectiondate, "week", week_start = getOption("lubridate.week.start", 1)),
         AgeAtCollection = trunc(time_length(interval(AllTests$date_of_birth,AllTests$collectiondate), "year"),0),
         AgeGrouping = cut(AgeAtCollection, breaks=c(-1,9,19,29,39,49,59,69,79,89,130), 
                           labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")),
         covid19_test_result = str_to_upper(covid19_test_result),
         sex = fct_collapse(sex, Male=c("M", "m", "MALE"), Female=c("F", "f", "FEMALE")),
         person_id = case_when(!is.na(person_id) ~ person_id,
                               is.na(person_id) ~ paste0(first_name,last_name,date_of_birth)),
         ID = paste0(first_name,last_name,date_of_birth))


# Final Tests DF Prep -----------------------------------------------------

#add flag for if row is most recent 2 weeks
AllTests <- AllTests %>%
  mutate(
         IsMostRecent2WeeksRolling = case_when(collectiondate %within% RollingRecent2Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         IsMostRecentWeekRolling = case_when(collectiondate %within% RollingRecentWeek ~ "Yes",
                                             TRUE ~ as.character("No")),
         IsMostRecent2WeekComparison = case_when(collectiondate %within% RollingRecent2WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")),
         IsMostRecent1WeekComparison = case_when(collectiondate %within% RollingRecent1WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")))

#collapse test_type levels
AllTests$test_type <- fct_collapse(AllTests$test_type, PCR = c("PCR", "RT-PCR"))

#create testing lab column for PCR test results delay

AllTests$sender <- as.character(AllTests$sender)
AllTests <- AllTests %>%
  mutate(TestingLab = case_when(lab=="CDPHE" ~"CDPHE",
                                str_detect(AllTests$sender,"QUEST") ~ "QUEST",
                                lab=="non-CDPHE (commercial)" ~ sender)
         )

AllTests$sender <- as.factor(AllTests$sender)
AllTests$TestingLab <- as.factor(AllTests$TestingLab)


PCRtests <- AllTests %>%
  filter(test_type=="PCR") 
PCRtests<- PCRtests %>%
  mutate(Lab = fct_collapse(TestingLab, "Routt County Providers"=c("SEC_RCPH","CDPHE","YVMC", "MAKO MEDICAL LABORATORIES", "EUROFINS", "AEGIS SCIENCES CORPORATION", 
                                                                   "LABCORP","UCH","FULGENT GENETICS", "CURATIVE", "ARUP LABORATORIES", 
                                                                   "STEAMBOAT EMERGENCY CENTER", "ADVANCED TESTING IN CLINICAL GENETICS (ATCG)",
                                                                   "PROVIDERFLATFILEUPLOAD")))

NonAntibodyTests <- AllTests %>%
  filter(test_type!="serology")

#dedup people who get antigen and PCR the same day

DeDupNonAntibodyTests <- NonAntibodyTests %>%
  group_by(ID, collectiondate, CollectionDateWeek, CollectionWeekStart,
           # , IsMostRecent2Weeks,
           IsMostRecent2WeeksRolling, IsMostRecentWeekRolling, IsMostRecent2WeekComparison, IsMostRecent1WeekComparison) %>% 
  summarise(DailyTestNum = row_number()) %>%
  filter(DailyTestNum==1)


# Clean up Cases Data Frame -----------------------------------------------

#colnames(CasesRaw)
CasesRaw[,c(1,4,8:9,15,18,19:31,34:37,39:43,46,50:52)] <- lapply(CasesRaw[,c(1,4,8:9,15,18,19:31,34:37,39:43,46,50:52)], as.factor)
CasesRaw[,c(7,10:14,16,47:49)] <- lapply(CasesRaw[,c(7,10:14,16,47:49)], as.Date.character)


##***Need to merge together specimen collection dates because not all PCR tests are specimen 1

AllCases <- CasesRaw %>%
  mutate(ReportedDateWeek = isoweek(reporteddate), 
         ReportedWeekStart = floor_date(reporteddate, "week", week_start = getOption("lubridate.week.start", 1)),
         CollectionDateWeek = isoweek(earliest_collectiondate), 
         CollectionWeekStart = floor_date(earliest_collectiondate, "week", week_start = getOption("lubridate.week.start", 1)),
         AgeGrouping = cut(age_at_reported, breaks=c(0,9,19,29,39,49,59,69,79,89,130), 
                           labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")))

AllCases$gender <- fct_explicit_na(AllCases$gender)


##Check that these are the correct Event IDs being removed. Not sure which CEDRS deleted or not

AllCases <- AllCases %>%
  mutate(AttributionWeekStart = case_when(is.na(CollectionWeekStart) ~ ReportedWeekStart,
                                          TRUE ~ CollectionWeekStart),
         AttributionDate = case_when(is.na(earliest_collectiondate) ~ reporteddate,
                                     TRUE ~ earliest_collectiondate),
         OnsetProxyWeekStart = floor_date(onsetdate_proxy_dist, "week", week_start = getOption("lubridate.week.start", 1)))

AllCases <- AllCases %>%
  mutate(IsMostRecent2Weeks = case_when(AttributionWeekStart==params$MostRecentWeekStart ~ "Yes",
                                        AttributionWeekStart==(as.Date(params$MostRecentWeekStart)-7) ~ "Yes",
                                        TRUE ~ as.character("No")),
         IsMostRecent2WeeksRolling = case_when(AttributionDate %within% RollingRecent2Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         IsMostRecentWeekRolling = case_when(AttributionDate %within% RollingRecentWeek ~ "Yes",
                                             TRUE ~ as.character("No")),
         IsMostRecent2WeekComparison = case_when(AttributionDate %within% RollingRecent2WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")),
         IsMostRecent1WeekComparison = case_when(AttributionDate %within% RollingRecent1WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No"))) 

ConfProbCases <- AllCases %>%
  filter(casestatus %in% c("confirmed", "probable"))


# #Checking for missing labs
# ConfProbCases %>%
#   filter(CaseStatus=="Probable") %>%
#   filter(AttributionWeekStart>"2020-10-15") %>%
#   filter(!is.na(Spec1_CollectionDate)) %>%
#   select(1,3:5,14:20,69:89)



# Case Outcome ------------------------------------------------------------

#Active = within 28 days of symptoms or test
#Recovered = >28 days since symptoms or test AND not hospitalized AND not died
#Hospitalized = any hospitalization
#Died = dies within 30 days (from CEDRS)

#10/18/2021 updated to use onsetdate_proxy for determining recovered

ConfProbCases <- ConfProbCases %>%
  mutate(CaseOutcome = case_when(outcome=="Patient died" ~ "Died",
                                 hospitalized=="TRUE" ~ "Hospitalized",
                                 difftime(params$DataThroughDate,onsetdate_proxy_dist)>28 ~ "Recovered",
                                 TRUE ~ as.character("Active"))) 



