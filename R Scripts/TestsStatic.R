#Creating static antigen test dataframe for compiling with new ELR Tests
#first authored 2/14/2021
#last updated 10/13/2021

library(tidyverse)
library(lubridate)
library(openxlsx)

#Need to add in Steamboat Emergency Center tests that aren't being sent to CEDRS - this will eventually be in SQL data entry table (rather than excel)
SECtestsRaw <- read.xlsx("COVID-Data-Files/Steamboat Emergency Center Tests Cummulative.xlsx", sheet = "List")

colnames(SECtestsRaw)
SECtestsRaw[,c(4,7:12,19,21)] <- lapply(SECtestsRaw[,c(4,7:12,19,21)], str_to_upper)

SECtestNames <- SECtestsRaw %>%
  select(-c(13:18)) %>%
  gather(TestType,x,7:12) %>%
  filter(!is.na(x)) %>%
  mutate(TestType=ifelse(TestType=="Total.Antibody","serology",TestType)) %>%
  select(-x)
SECtestResults <- SECtestsRaw %>%
  select(-c(4,7:12,19:21)) %>%
  gather(TestType,MappedResult,6:11) %>%
  mutate(TestType=str_sub(TestType,1,-8)) %>%
  filter(!is.na(MappedResult)) %>%
  mutate(MappedResult=str_to_lower(MappedResult))
SECtests <- SECtestNames %>%
  left_join(SECtestResults, by = c("first_name", "last_name", "Date_of_Birth", "TestType", "CollectionDate", "ResultDate")) %>%
  mutate(submitter="SEC_RCPH", PerformingLab="Other",
         ReportDate=ResultDate,
         person_id=paste0(first_name,last_name,Date_of_Birth)) %>%
  #Removing JialeathCarroll and PatrickHagan results because those showed up in CEDRS
  filter(person_id!="JialeathCarroll32128") %>%
  filter(person_id!="PatrickHagan33492") %>%
  select(-person_id) %>%
  rename(date_of_birth = Date_of_Birth, test_type = TestType, covid19_test_result = MappedResult, receivedate = ReportDate, 
         collectiondate = CollectionDate, resultdate = ResultDate) %>%
  select(1:6,10:12,14)

colnames(SECtests)
SECtests[,c(4,7:9)] <- lapply(SECtests[,c(4,7:9)], as.factor)
SECtests[,c(3,5:6,10)] <- lapply(SECtests[,c(3,5:6,10)], as.Date.numeric, origin = "1899-12-30")

##***12/27/2020: removing SEC manual entry tests after 11/24 because it appears immunogenomics is reporting in.
SECtests <- SECtests %>%
  filter(collectiondate<"2020-11-25")

#create list of just Routt county resident SEC tests, pivot to match style of TestsRaw file
SECtestsRoutt <- SECtests %>%
  filter(`Routt.Resident?`=="Y") 


##AntigenTests

SMCantigen <- read.xlsx("COVID-Data-Files/SMCantigenLog.xlsx")
SRMCantigen <- read.xlsx("COVID-Data-Files/SouthRouttAntigenLog.xlsx")
SSFMantigen <- read.xlsx("COVID-Data-Files/SSFMantigenLog.xlsx")


colnames(SMCantigen)
SMCantigen <- SMCantigen %>%
  mutate(`Routt.Resident?` = case_when(str_detect(Address,"80428|80467|80469|80477|80479|80483|80487|80488|81639") ~ "Y",
                                       TRUE ~ as.character("N")),
         submitter="SMC_RCPH", PerformingLab="Other",
         resultdate=CollectionDate,
         receivedate=CollectionDate,
         person_id=paste0(FirstName,LastName,DOB)) %>%
  rename(first_name = FirstName, last_name = LastName, date_of_birth = DOB, test_type = TestType, covid19_test_result = TestResult, 
         collectiondate = CollectionDate, sex = Sex, race = Race) %>%
  select(1:6,8, 13:19,21:22)

SMCantigen[,c(2:3,5:6,8:11,13:14)] <- lapply(SMCantigen[,c(2:3,5:6,8:11,13:14)], as.factor)
SMCantigen[,c(1,4,15:16)] <- lapply(SMCantigen[,c(1,4,15:16)], as.Date.numeric,  origin = "1899-12-30")

SMCantigenRoutt <- SMCantigen %>%
  filter(`Routt.Resident?`=="Y")

#clean up bad data entry
SRMCantigen$DOB <-  replace(SRMCantigen$DOB, c(44), c("26129"))
SRMCantigen$collection.date <-  replace(SRMCantigen$collection.date, c(76), c("44117"))
SRMCantigen$DOB <- as.numeric(SRMCantigen$DOB)
SRMCantigen$collection.date <- as.numeric(SRMCantigen$collection.date)

SRMCantigen <- SRMCantigen %>%
  mutate(`Routt.Resident?` = case_when(str_detect(address,"80428|80467|80469|80477|80479|80483|80487|80488|81639") ~ "Y",
                                       TRUE ~ as.character("N")),
         submitter="SRMC_RCPH", PerformingLab="Other",
         resultdate=collection.date,
         receivedate=collection.date,
         person_id=paste0(First.name,Last.name,DOB)) %>%
  rename(first_name = First.name, last_name = Last.name, date_of_birth = DOB, test_type = type, covid19_test_result = result, 
         collectiondate = collection.date) %>%
  select(2:8, 12:13, 15, 17:18, 20:21)

colnames(SRMCantigen)
SRMCantigen[,c(4:6,8,10:12)] <- lapply(SRMCantigen[,c(4:6,8,10:12)], as.factor)
SRMCantigen[,c(3,9,13:14)] <- lapply(SRMCantigen[,c(3,9,13:14)], as.Date.numeric, origin = "1899-12-30")

SRMCantigenRoutt <- SRMCantigen %>%
  filter(`Routt.Resident?`=="Y")

SSFMantigen <- SSFMantigen %>%
  mutate(`Routt.Resident?` = case_when(str_detect(Zip,"80428|80467|80469|80477|80479|80483|80487|80488|81639") ~ "Y",
                                       TRUE ~ as.character("N")),
         submitter="SSFM_RCPH", PerformingLab="Other",
         resultdate=Collection.Date,
         receivedate=Collection.Date,
         person_id=paste0(First.Name,Last.Name,Date.of.Birth)) %>%
  rename(first_name = First.Name, last_name = Last.Name, date_of_birth = Date.of.Birth, collectiondate = Collection.Date, 
         covid19_test_result = Test.Result, sex = Sex, test_type = Test.Type, address = Address.1, address2 = Address.2, city = City,
         zipcode = Zip, state = State, race =`Race/Eth`) %>%
  select(4:13,17,21:23,25:26,28:29)

colnames(SSFMantigen)
SSFMantigen[,c(3,8:12,14:16)] <- lapply(SSFMantigen[,c(3,8:12,14:16)], as.factor)
SSFMantigen[,c(4,13,17:18)] <- lapply(SSFMantigen[,c(4,13,17:18)], as.Date.numeric, origin = "1899-12-30")
# # CODE BELOW IS FOR DATES FORMATTED WITHOUT SLASHES - 10/13/20 MANUALLY UPDATED EXCEL FILE BECAUSE DATES SENT INCONSISTENTLY
# SSFMantigen[,7] <- as.character(SSFMantigen[,7])
# SSFMantigen <- SSFMantigen %>%
#   mutate(DOByear = str_sub(Date_of_Birth,-4),
#          DOBremain = str_sub(Date_of_Birth,end=-5),
#          DOBday = str_sub(DOBremain,-2),
#          DOBmonthPrep = paste0("0",str_sub(DOBremain,end=-3)),
#          DOBmonth = str_sub(DOBmonthPrep,-2),
#          Date_of_Birth = paste0(DOBmonth,"/",DOBday,"/",DOByear)) %>%
#   select(-c(DOBremain, DOByear, DOBday, DOBmonthPrep, DOBmonth))
# SSFMantigen$Date_of_Birth <- as.Date.character(SSFMantigen$Date_of_Birth, format = "%m/%d/%Y")

SSFMantigenRoutt <- SSFMantigen %>%
  filter(`Routt.Resident?`=="Y")

#create one static antigen DF
StaticTestLogs <- SECtestsRoutt %>%
  bind_rows(SRMCantigenRoutt) %>%
  bind_rows(SMCantigenRoutt) %>%
  bind_rows(SSFMantigenRoutt)

colnames(StaticTestLogs)
colnames(TestsRaw)

StaticTestLogs <- StaticTestLogs %>%
  select(1:3,5:14,20:23)


#Clean up StaticTestLogs file to be compatible with ELR tests file from LPHA portal
#First authored 10/12/2021 by Nicole Harty
#Last update: 10/13/2021


StaticTestLogs <- StaticTestLogs %>%
  mutate(serology_test_type = test_type,
         test_type = fct_collapse(test_type, serology=c("IgA","IgG","IgM","Total Antibody"), 
                                  antigen=c("Antigen", "Sofia 2 SARS Antigen", "SOFIA 2 QUIDEL SARS ANTIGEN", "Antigen for COVID-19",
                                            "RAPID POC SARS/COVID 19 ANTIGEN-QUIDEL/SOFIA", "Sofia2 Quidel SARS Antigen")),
         lab="non-CDPHE (commercial)",
         dateadded = NA,
         county = "ROUTT",
         covid19_test_result = fct_collapse(covid19_test_result, Positive=c("POSITIVE", "Positive"),
                                            Negative=c("NEGATIVE", "negative", "Negative")))


#Save StaticTestLogs to both BOH Weekly report directory AND Shiny app directory
saveRDS(StaticTestLogs, file = "COVID-Data-Files/StaticTestLogs.RDS")

#saveRDS(StaticTestLogs, file = "Internal Dashboard v1/Data/StaticTestLogs.RDS")
