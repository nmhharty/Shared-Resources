#Script to load and clean all test results and cases data including that from CEDRS and Steamboat Emergency Center (SEC) tests that are not being entered in CEDRS
#First authored 9/10/2020 by Nicole Harty
#Last update: 9/17/2021
##data sourced from Excel files. Will need to change to sourcing from SQL as appropriate when database set up

library(tidyverse)
library(lubridate)
library(openxlsx)


TestsRawCSV <- read.csv("../Shared Resources/Data Files/CEDRS_COVIDELRTests.csv", skip = 3)
CasesRawXLSX <- read.xlsx("../Shared Resources/Data Files/CEDRS_NovelCoronavirusListing.xlsx", startRow = 4)

#add flag for if row is most recent 2 weeks
#UNCOMMENT BELOW LINE WHEN TROUBLESHOOTING. KEEP COMMENTED WHEN RUNNING PRODUCTION - PARAMETER IS SET IN MARKDOWN
DataThroughDate <- as.Date.character("2021-10-18")
MostRecentWeekStart = as.Date.character("2021-10-11")

RecentWeek <- interval(ymd(MostRecentWeekStart), ymd(MostRecentWeekStart+6))
Recent2Week <- interval(ymd(MostRecentWeekStart-7), ymd(MostRecentWeekStart+6))
RollingRecentWeek <- interval((DataThroughDate-6), DataThroughDate)
RollingRecent2Week <- interval((DataThroughDate-13), DataThroughDate)
RollingRecent2WeekComparison <- interval((DataThroughDate-20), (DataThroughDate-7))
RollingRecent1WeekComparison <- interval((DataThroughDate-13), (DataThroughDate-7))
RollingRecent4Week <- interval((DataThroughDate-27), DataThroughDate)




# Clean up Tests Data Frame -----------------------------------------------
colnames(TestsRawCSV)
TestsRawCSV[,c(1:2,5:8,11:13,15:20,23:24)] <- lapply(TestsRawCSV[,c(1:2,5:8,11:13,15:20,23:24)], as.factor)
TestsRawCSV[,c(3,14,21,22)] <- lapply(TestsRawCSV[,c(3,14,21,22)], as.Date.character, "%m/%d/%Y")



# Antigen test Logs -------------------------------------------------------

#Add Routt antigen and SEC tests to TestsRaw
# StaticTestLogs <- readRDS("StaticTestLogs.RDS")
# AllTests <- TestsRaw %>%
#   bind_rows(StaticTestLogs) 

#Add Week and date of week start for collection dates, age at test collection, age grouping
#create collapsed groups of TestType factors and gender
levels(AllTests$TestType)
AllTestsCSV <- TestsRawCSV %>%
  mutate(CollectionDateWeek = isoweek(CollectionDate), 
         CollectionWeekStart = floor_date(CollectionDate, "week", week_start = getOption("lubridate.week.start", 1)),
         AgeAtCollection = trunc(time_length(interval(TestsRawCSV$Date_of_Birth,TestsRawCSV$CollectionDate), "year"),0),
         AgeGrouping = cut(AgeAtCollection, breaks=c(0,9,19,29,39,49,59,69,79,89,130), 
                           labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")),
         TestTypeGroup = fct_collapse(TestType, Antibody=c("IgA","IgG","IgM","Total Antibody"), 
                                      Antigen=c("Antigen", "Sofia 2 SARS Antigen", "SOFIA 2 QUIDEL SARS ANTIGEN", "Antigen for COVID-19",
                                                "RAPID POC SARS/COVID 19 ANTIGEN-QUIDEL/SOFIA", "Sofia2 Quidel SARS Antigen")),
         MappedResult = str_to_upper(MappedResult),
         ID = paste0(first_name,last_name,Date_of_Birth))

#Adding test Duplicate Flags: 
# ##sequence of tests by type, sequence of tests after first positive PCR, number of days from positive PCR/antigen to follow-up test
# AllTests %>%
#   filter(last_name=="BOWMAN") %>%
#   select(first_name, last_name, CollectionDate, TestType, TestTypeGroup, MappedResult, ID) %>%
#   group_by(ID, TestType, MappedResult) %>%
#   mutate(TestTypeSeq = row_number(),
#          ConfirmProbCaseCriteria = case_when(TestTypeGroup=="RT-PCR"&TestTypeSeq==1&MappedResult=="Positive" ~ "Yes",
#                                              TestTypeGroup=="Antigen"&CollectionDate>"2020-08-17"&TestTypeSeq==1&MappedResult=="Positive" ~ "Yes",
#                                              TRUE ~ NA_character_
#          ),
#          RepeatWithin90 = case_when(TestTypeSeq>1 ~ "Yes",
#                                     TRUE ~ NA_character_)
#   )


# Final Tests DF Prep -----------------------------------------------------

#add flag for if row is most recent 2 weeks
AllTestsCSV <- AllTestsCSV %>%
  mutate(
    # IsMostRecent2Weeks = case_when(CollectionDateWeek==MostRecentWeekNumber ~ "Yes",
    #                                     CollectionDateWeek==(MostRecentWeekNumber-1) ~ "Yes",
    #                                     TRUE ~ as.character("No")),
         IsMostRecent2WeeksRolling = case_when(CollectionDate %within% RollingRecent2Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         IsMostRecentWeekRolling = case_when(CollectionDate %within% RollingRecentWeek ~ "Yes",
                                             TRUE ~ as.character("No")),
         IsMostRecent2WeekComparison = case_when(CollectionDate %within% RollingRecent2WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")),
         IsMostRecent1WeekComparison = case_when(CollectionDate %within% RollingRecent1WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")))

PCRtestsCSV <- AllTestsCSV %>%
  filter(TestTypeGroup=="RT-PCR")

NonAntibodyTestsCSV <- AllTestsCSV %>%
  filter(TestTypeGroup!="Antibody")

# #dedup people who get antigen and PCR the same day
DeDupNonAntibodyTestsCSV <- NonAntibodyTestsCSV %>%
  group_by(ID, CollectionDate, CollectionDateWeek, CollectionWeekStart,
           # , IsMostRecent2Weeks,
           IsMostRecent2WeeksRolling, IsMostRecentWeekRolling, IsMostRecent2WeekComparison, IsMostRecent1WeekComparison
  ) %>%
  summarise(DailyTestNum = row_number()) %>%
  filter(DailyTestNum==1)



# Google Sheets Cases info and Visitors -----------------------------------

library(googlesheets4)

# Satic old weeks'data (as of 9/24/2021, this is through week of Aug 9)
# StaticPHtracking <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
#                          sheet = 1)
# for (i in 2:46) {
#   df <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
#                    sheet = i)
#   df[,1:12] <- lapply(df[,1:12], as.character)
#   StaticPHtracking <- StaticPHtracking %>%
#     rbind(df)
# 
# }
# saveRDS(StaticPHtracking, file = "StaticPHtracking.RDS")

StaticPHtracking <- readRDS("StaticPHtracking.RDS")

gs4_auth(email = "routtcodph@gmail.com")

nmaxGsheetTabs <- isoweek(Sys.Date())+15
PHtracking <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
                 sheet = 47, range = "A:M")
for (i in 48:nmaxGsheetTabs) {
   df <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
                      sheet = i, range = "A:M")
   df[,1:12] <- lapply(df[,1:12], as.character)
   PHtracking <- PHtracking %>%
     rbind(df)

}

PHtracking <- PHtracking %>%
  rbind(StaticPHtracking)

PHtracking <- PHtracking %>%
  rename(ReportedDate = "Date and Time of Notification", Name = "Patient Name", StaffAssigned = "Person Following up",
         InvestigationStartDate = "Date and Time of Investigation Start", Spec1_CollectionDate = "Specimen Collection Date",
         InCEDRS = "Is case in CEDRS?", CEDRSentryDate = "Date Entered into CEDRS") %>%
  mutate(ReportedDateWeek = isoweek(ReportedDate), 
         ReportedWeekStart = floor_date(ReportedDate, "week", week_start = getOption("lubridate.week.start", 1)),
         CollectionDateWeek = isoweek(Spec1_CollectionDate), 
         CollectionWeekStart = floor_date(Spec1_CollectionDate, "week", week_start = getOption("lubridate.week.start", 1)))

PHtracking$`Exposure Category` <- as.factor(PHtracking$`Exposure Category`)
levels(PHtracking$`Exposure Category`)

PHtracking <- PHtracking %>%
  mutate(ExposureCatCollapse = fct_collapse(`Exposure Category`, Travel=c("Travel, Known Contact","Travel, No Known Contact"),
                                            "Known Contact"=c("Known Contact", "Known Contact - Social", "Known Contact - Employment", 
                                                              "Known Contact - School", "Known Contact - School or Child Care", 
                                                              "Known Contact - Sports")))

PHtracking <- PHtracking %>%
  mutate(
    # IsMostRecent2Weeks = case_when(CollectionDateWeek==MostRecentWeekNumber ~ "Yes",
    #                                     CollectionDateWeek==(MostRecentWeekNumber-1) ~ "Yes",
    #                                     TRUE ~ as.character("No")),
         AttributionWeekStart = case_when(is.na(CollectionWeekStart) ~ ReportedWeekStart,
                                          TRUE ~ CollectionWeekStart),
         AttributionDate = case_when(is.na(Spec1_CollectionDate) ~ ReportedDate,
                                     TRUE ~ Spec1_CollectionDate),
         IsMostRecent2WeeksRolling = case_when(AttributionDate %within% RollingRecent2Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         IsMostRecentWeekRolling = case_when(AttributionDate %within% RollingRecentWeek ~ "Yes",
                                             TRUE ~ as.character("No")),
         IsMostRecent2WeekComparison = case_when(AttributionDate %within% RollingRecent2WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")),
         IsMostRecent1WeekComparison = case_when(AttributionDate %within% RollingRecent1WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")))
PHtracking$AttributionWeekStart <- as.Date(as.POSIXct(PHtracking$AttributionWeekStart))
PHtracking$AttributionDate <- as.Date(as.POSIXct(PHtracking$AttributionDate))


#Visitor definition I'm using on Google Sheet is ___________
VisitorCases <- PHtracking %>%
  filter(`Resident or No`=="Visitor")


# Clean up Cases Data Frame -----------------------------------------------

#*****new column, "Re-Infection Y/N", will be added to the far right of several COVID Reports on Wednesday, October 28th****

colnames(CasesRaw)
CasesRawXLSX[,c(2:3,6:7,9:12,14:16,19:28,32:37,39:43,46,49,52:68,70:76,78:79,82:88,90:91,94:100,102:103,110:117,119,121,122)] <- 
  lapply(CasesRawXLSX[,c(2:3,6:7,9:12,14:16,19:28,32:37,39:43,46,49,52:68,70:76,78:79,82:88,90:91,94:100,102:103,110:117,119,121,122)], as.factor)
CasesRawXLSX[,c(4:5,8,13,17,38,44:45,47:48,50:51,69,77,80:81,89,92:93,101,104,118,120)] <- 
  lapply(CasesRawXLSX[,c(4:5,8,13,17,38,44:45,47:48,50:51,69,77,80:81,89,92:93,101,104,118,120)], as.Date.numeric, origin = "1899-12-30")


##***Need to merge together specimen collection dates because not all PCR tests are specimen 1

AllCasesXLSX <- CasesRawXLSX %>%
  mutate(ReportedDateWeek = isoweek(ReportedDate), 
         ReportedWeekStart = floor_date(ReportedDate, "week", week_start = getOption("lubridate.week.start", 1)),
         CollectionDateWeek = isoweek(Spec1_CollectionDate), 
         CollectionWeekStart = floor_date(Spec1_CollectionDate, "week", week_start = getOption("lubridate.week.start", 1)),
         AgeGrouping = cut(Age, breaks=c(0,9,19,29,39,49,59,69,79,89,130), 
                           labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")))

AllCasesXLSX$Gender <- fct_explicit_na(AllCasesXLSX$Gender)




##Check that these are the correct Event IDs being removed. Not sure which CEDRS deleted or not

AllCasesXLSX <- AllCasesXLSX %>%
  mutate(AttributionWeekStart = case_when(is.na(CollectionWeekStart) ~ ReportedWeekStart,
                                          TRUE ~ CollectionWeekStart),
         AttributionDate = case_when(is.na(Spec1_CollectionDate) ~ ReportedDate,
                                     TRUE ~ Spec1_CollectionDate))

AllCasesXLSX <- AllCasesXLSX %>%
  mutate(IsMostRecent2Weeks = case_when(AttributionWeekStart==MostRecentWeekStart ~ "Yes",
                                        AttributionWeekStart==(MostRecentWeekStart-7) ~ "Yes",
                                        TRUE ~ as.character("No")),
         IsMostRecent2WeeksRolling = case_when(AttributionDate %within% RollingRecent2Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         IsMostRecentWeekRolling = case_when(AttributionDate %within% RollingRecentWeek ~ "Yes",
                                             TRUE ~ as.character("No")),
         IsMostRecent2WeekComparison = case_when(AttributionDate %within% RollingRecent2WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")),
         IsMostRecent1WeekComparison = case_when(AttributionDate %within% RollingRecent1WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No"))) 

ConfProbCasesXLSX <- AllCasesXLSX %>%
  filter(CaseStatus %in% c("Confirmed", "Probable")) %>%
  mutate(CountsTowardTotal = case_when(CaseStatus=="Probable"&Spec1_Test1_Name=="Antigen for COVID-19" ~ "Yes",
                                       CaseStatus=="Confirmed" ~ "Yes",
                                       TRUE ~ as.character("No")))


# #Checking for missing labs
# ConfProbCases %>%
#   filter(CaseStatus=="Probable") %>%
#   filter(AttributionWeekStart>"2020-10-15") %>%
#   filter(!is.na(Spec1_CollectionDate)) %>%
#   select(1,3:5,14:20,69:89)



# Hospitalizations --------------------------------------------------------

Hospit <- read.xlsx("HospitalizationData.xlsx")
Hospit[,c(5,8:10)] <- 
  lapply(Hospit[,c(5,8:10)], as.Date.numeric, origin = "1899-12-30")

Hospit <- Hospit %>%
  mutate(RouttResident=`Routt.Resident?`,
         CurrentlyHospitalized = case_when(is.na(Hospital.Discharge.Date) ~ "Yes",
                                           TRUE ~ as.character("No")),
         CurrentlyAtYVMC = case_when(Hospital.Name!="Yampa Valley Medical Center" ~ "No",
                                     (Hospital.Name=="Yampa Valley Medical Center"&CurrentlyHospitalized=="No") ~ "No",
                                     (Hospital.Name=="Yampa Valley Medical Center"&!is.na(Hospital.Transfer.Date)) ~ "No",
                                     TRUE ~ as.character("Yes")),
         AdmissionInterval = interval(ymd(Hospital.Admission.Date), ymd(Hospital.Discharge.Date)),
         IsMostRecent2Weeks = case_when(Hospital.Admission.Date %within% Recent2Week ~ "Yes",
                                        TRUE ~ as.character("No")),
         IsMostRecent2WeeksRolling = case_when(Hospital.Admission.Date %within% RollingRecent2Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         IsMostRecentWeekRolling = case_when(Hospital.Admission.Date %within% RollingRecentWeek ~ "Yes",
                                             TRUE ~ as.character("No")),
         IsMostRecent2WeekComparison = case_when(Hospital.Admission.Date %within% RollingRecent2WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No")),
         IsMostRecent1WeekComparison = case_when(Hospital.Admission.Date %within% RollingRecent1WeekComparison ~ "Yes",
                                                 TRUE ~ as.character("No"))
         ) %>%
  select(-`Routt.Resident?`)

Hospit %>% 
  filter(RouttResident=="Yes"&IsMostRecentWeekRolling=="Yes") %>% 
  summarise(Count = n()) %>%
  mutate(HospitCountCat = case_when(Count==0 ~ "0",
                                    Count<5 ~ "1-4",
                                    Count<10 ~ "5-9",
                                    Count>10 ~ "10+"))


#Dec 2020: state updated the dial for hospitalizations
#Hospitalizations levels are 0 days with new hospitalizations (green)
##at least one day with 1-2 new hospitalizations (yellow), 
##and at least one day with more than 2 new hospitalizations (red).

Hospit %>%
  filter(`RouttResident`=="Yes", IsMostRecent2WeeksRolling=="Yes") %>%
  group_by(Hospital.Admission.Date) %>%
  summarise(DailyHospitalizations=n()) %>%
  mutate(MaxHospit = max(DailyHospitalizations)) %>%
  slice(1)


#COPHS Hospitalization Data
COPHS <- read.delim("cophs_Routt.txt", sep = "|") %>%
  mutate(PersonID = paste0(First.Name,Last.Name,DOB..MM.DD.YYYY.))

COPHS <- mutate_all(COPHS, list(~na_if(.,"")))

colnames(COPHS)
COPHS[,c(5:7,16,20)] <- lapply(COPHS[,c(5:7,16,20)], as.Date.character)

COPHS <- COPHS %>% 
  mutate( CurrentlyHospitalized = case_when(is.na(Discharge.Transfer..Death.Date...MM.DD.YYYY.) ~ "Yes",
                                            TRUE ~ as.character("No")),
          CurrentlyAtYVMC = case_when(Facility.Name!="Yampa Valley Medical Center" ~ "No",
                                      (Facility.Name=="Yampa Valley Medical Center"&CurrentlyHospitalized=="No") ~ "No",
                                      TRUE ~ as.character("Yes")),
          AdmissionInterval = interval(ymd(Hospital.Admission.Date...MM.DD.YYYY.), 
                                       as.Date.numeric(ifelse(is.na(ymd(Discharge.Transfer..Death.Date...MM.DD.YYYY.)),ymd(Sys.Date()),
                                                              ymd(Discharge.Transfer..Death.Date...MM.DD.YYYY.)), origin = "1970-01-01")),
          IsMostRecent2Weeks = case_when(Hospital.Admission.Date...MM.DD.YYYY. %within% Recent2Week ~ "Yes",
                                         TRUE ~ as.character("No")),
          IsMostRecent2WeeksRolling = case_when(Hospital.Admission.Date...MM.DD.YYYY. %within% RollingRecent2Week ~ "Yes",
                                                TRUE ~ as.character("No")),
          IsMostRecentWeekRolling = case_when(Hospital.Admission.Date...MM.DD.YYYY. %within% RollingRecentWeek ~ "Yes",
                                              TRUE ~ as.character("No")),
          IsMostRecent2WeekComparison = case_when(Hospital.Admission.Date...MM.DD.YYYY. %within% RollingRecent2WeekComparison ~ "Yes",
                                                  TRUE ~ as.character("No")),
          IsMostRecent1WeekComparison = case_when(Hospital.Admission.Date...MM.DD.YYYY. %within% RollingRecent1WeekComparison ~ "Yes",
                                                  TRUE ~ as.character("No")))


#Total number residents hospitalized
COPHS %>%
  summarise(n_distinct(PersonID))


# Case Outcome ------------------------------------------------------------

#Active = within 28 days of symptoms or test
#Recovered = >28 days since symptoms or test AND not hospitalized AND not died
#Hospitalized = any hospitalization
#Died = dies within 30 days (from CEDRS)

ConfProbCases <- ConfProbCases %>%
  filter(CaseStatus!="Suspect") %>%
  mutate(OutcomeStartDate = case_when(!is.na(OnsetDate) ~ OnsetDate,
                                      is.na(OnsetDate)&!is.na(Spec1_CollectionDate) ~ Spec1_CollectionDate,
                                      is.na(Spec1_CollectionDate) ~ ReportedDate),
         CaseOutcome = case_when(Outcome=="Patient died (finding)" ~ "Died",
                                 Hospitalized=="Yes" ~ "Hospitalized",
                                 difftime(DataThroughDate,OutcomeStartDate)>14 ~ "Recovered",
                                 TRUE ~ as.character("Active"))) 



# Outbreaks ---------------------------------------------------------------

Outbreaks <- range_read("https://docs.google.com/spreadsheets/d/1mLEWVyOADxp4FsVBvGjKpJFM97jpImAhsxQVUI1Ga1Q/edit#gid=167774008",
                        sheet = "Dashboard Source Sheet")
Outbreaks <- Outbreaks %>%
  mutate(IsMostRecent4WeeksRolling = case_when(`Start Date` %within% RollingRecent4Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         ActiveClosed = case_when(`End Date (Update as Needed)`<DataThroughDate ~ "Closed",
                                  TRUE ~ as.character("Active")))

SchoolOutbreaks <- Outbreaks %>%
  filter(Type2=="School/Childcare", Status!="Under Investigation") %>%
  mutate("Academic Year" = case_when(`Start Date` < "2021-06-01" ~ "2020-21",
                                     `Start Date` < "2021-08-01" ~ "Summer 2021",
                                     `Start Date` < "2022-06-01" ~ "2021-22 to date",
                                     `Start Date` < "2022-08-01" ~ "Summer 2022 to date",
                                     `Start Date` < "2023-06-01" ~ "2022-23 to date",
                                     TRUE ~ as.character("Not during academic year.")),
         "School or District" = case_when(str_detect(`Outbreak Name`,"SSHS")|str_detect(`Outbreak Name`,"SSMS")|
                                            str_detect(`Outbreak Name`,"Strawberry Park")|str_detect(`Outbreak Name`,"Soda Creek")|
                                            str_detect(`Outbreak Name`,"Sleeping Giant") ~ "Steamboat Springs School District",
                                          str_detect(`Outbreak Name`,"SOROCO")|str_detect(`Outbreak Name`,"Yampa")|
                                            str_detect(`Outbreak Name`,"South Routt") ~ "SOROCO School District",
                                          str_detect(`Outbreak Name`,"North Routt") ~ "North Routt Community Charter School",
                                          str_detect(`Outbreak Name`,"Steamboat Mountain School")|
                                            str_detect(`Outbreak Name`, "Emerald Mountain School") ~ "Steamboat Mountain School",
                                            str_detect(`Outbreak Name`,"Hayden") ~ "Hayden School District",
                                          TRUE ~ as.character("Other")))

SchoolOutbreaks$`Academic Year` <- factor(SchoolOutbreaks$`Academic Year`, levels = c("2020-21", "Summer 2021", "2021-22 to date", "Summer 2022 to date", 
                                                      "2022-23 to date", "Not during academic year."))

# Symptoms ----------------------------------------------------------------

#Symptom onset available in cases data, details of symptoms only in Surveillance Report (and that has difficultly running)

## BELOW IS FOR USING DR J FOR SYMPTOMS
library(DBI)
library(odbc)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server=s-vm10-ph01;database=JUSTINA", timeout = 10)
DrJsymptoms <- dbGetQuery(con,
                          "SELECT  event_id, first_name, last_name, address_county, symptomatic, symptom_onset_date, final_disposition, date_opened FROM JUSTINA.dbo.patient WHERE address_county = 'Routt' and patient_type != 'suspect'")
DrJsymptoms$event_id <- as.numeric((DrJsymptoms$event_id))

#Create a dataframe of cases within the last 10 days of cases that have not been interviewed yet and/or the case has not been marked as complete. 
##The goal is to get a list of cases still in their isolation period for whom symptom information is unavailable.
RecentCasesSymptUNK <- DrJsymptoms %>%
  filter(final_disposition==""&(symptomatic=="unknown"|is.na(symptomatic))) %>%
  filter(date_opened>=(Sys.Date()-10)&!is.na(event_id))

#Below is the dataframe I create to do my analysis. I can subset it to give me symptomatic, asymptomatic, etc. 
##First I filter the SQL query above to give me just event_id where final_disposition is reached completed, reached not complete, or unreachable. 
##Then I bind (union in SQL-speak) the dataframe above that is the lits of event_id with symptom status unknown. 
##Then I left join the CEDRS Novel Cornavirus report that has been filtered to have just probable and confirmed cases.
SymptomDF <- DrJsymptoms %>%
  filter(final_disposition %in% c("reached_completed", "reached_not_completed", "unreachable")) %>%
  rbind(RecentCasesSymptUNK) %>%
  left_join(ConfProbCases %>%
              select(EventID, ProfileID, CaseStatus, Gender, BirthDate,
                     OnsetDate, Spec1_CollectionDate, AttributionDate, ReportedDate, AttributionWeekStart), by = c("event_id" = "EventID")) %>%
  mutate(SymptomsBeforeTest = case_when(symptom_onset_date<=AttributionDate ~ "Yes",
                                        TRUE ~ as.character("No")))


##THIS IS FOR USING CEDRS
SymptomOverview <- ConfProbCases %>%
  select(EventID, ProfileID, CaseStatus, Gender, BirthDate, 
         OnsetDate, Spec1_CollectionDate, AttributionDate, ReportedDate, AttributionWeekStart) %>%
  mutate(SymptomsBeforeTest = case_when(OnsetDate<=AttributionDate ~ "Yes",
                                        TRUE ~ as.character("No")),
         SymptomsEver = case_when(is.na(OnsetDate) ~ "No",
                                  TRUE ~ as.character("Yes")))


# ConfProbCases %>%
#   filter(Outcome=="Patient died (finding)") %>%
#   select(EventID, FirstName, LastName, AttributionDate)
# 
# ConfProbCases %>%
#   filter(AttributionDate>"2021-02-07") %>%
#   count()

# PtIZ$zip_code <-  str_sub(PtIZ$zip_code, end = 5)
# 
# PtIZ %>%
#   group_by(zip_code) %>%
#   summarise(count = n_distinct(patient_id)) %>%
#   mutate(PercentTotalVaxPeople = (count/(PtIZ %>%
#                                           summarise(N = n_distinct(patient_id)) %>%
#                                            pull())*100)) %>%
#   filter(zip_code %in% c("80428", "80467", "80469", "80477", "80479", "80483", "80487", "80488", "81639")) %>%
#   left_join(data.frame(zip_code=c("80428", "80467", "80469","80479", "80483","81639", "80477", "80487", "80488"),
#                        Pop=c(706,2224,337,48,585,2463,155,16745,157)),
#             by = c("zip_code")) %>%
#   mutate(TotalPop = Pop/23509*100)



