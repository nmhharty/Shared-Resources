#Script to load Schools Case Report data for analysis
#First authored 10/6/2021 by Nicole Harty
#Last update: 11/10/2021

library(tidyverse)
library(lubridate)
library(kableExtra)
library(ggplot2)
library(plotly)
library(janitor)
library(imager)
library(googlesheets4)
library(DBI)
library(odbc)



# PUll Dr J School Data ---------------------------------------------------

con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server=s-vm10-ph01;database=JUSTINA", timeout = 10)
#DrJ dataframe for joining to CEDRS
DrJSchoolCases <- dbGetQuery(con, "SELECT DateOpened, EventID, FirstName, LastName, FinalDisposition, Age, DOB, SymptomOnsetDate, IsoStart, IsoEnd, SchoolType, SchoolName, 
                                SchoolInfoNotes, ZIP 
                        FROM
                        (SELECT CAST(pt.date_opened as Date) as DateOpened, CAST(pt.event_id as int) as EventID, CAST(first_name as varchar(256)) as FirstName, 
		                       CAST(last_name as varchar(256)) as LastName, CAST(final_disposition as varchar(256)) as FinalDisposition, CAST(age as int) as Age, 
		                       CAST(dob as Date) as DOB, CAST(symptom_onset_date as Date) as SymptomOnsetDate, CAST(isolation_start_date as Date) as IsoStart, 
		                       CAST(isolation_end_date as Date) as IsoEnd, CAST(school_name as varchar(4000)) as SchoolName, 
		                       CAST(school_info_notes as varchar(4000)) as SchoolInfoNotes, CAST(school_type as varchar(256)) as SchoolType, 
		                       CAST(address_zip as varchar(256)) as ZIP
                    		FROM JUSTINA.dbo.patient pt
                    		    WHERE address_county = 'Routt'
                    		          and final_disposition <> 'duplicate' and final_disposition <> 'ooj' and final_disposition <> 'not_a_case'
									  and pt.date_opened > '2021-08-15') a")
DrJSchoolCases$EventID <- as.numeric((DrJSchoolCases$EventID))


# School COVID Case Report Form Data Wrangling ----------------------------
#load data from Google Spreadsheet
gs4_auth(email = "routtcodph@gmail.com")
SchoolCaseReportForm <- range_read("https://docs.google.com/spreadsheets/d/1IS5XZL6i4xsnJEegykJYlik6BARRfetXbrQvpMYRd4g/edit?resourcekey#gid=164583539",
                                   sheet = 1, range = "A:P") %>%
  filter(Timestamp>"2021-10-08")

#clean up column names
colnames(SchoolCaseReportForm) <- c("Timestamp","Email","School","Name", "DOB","PrimaryContact","PrimaryContactPhoneNumber", "TestDate", "TestType",
                                    "Role","Grade","LastDaySchool","SymptomOnset","Extracurriculars","Notes","ContactPersonEmail")

SchoolCaseReportForm$School <- factor(SchoolCaseReportForm$School, levels = c("Hayden Elementary","Hayden Secondary","North Routt Charter","Sleeping Giant",
                                                                              "Soda Creek", "Soda Creek Elementary","South Routt Elementary","SOROCO High",
                                                                              "SOROCO Middle","SOROCO Preschool at Yampa","Steamboat High","Steamboat Middle",
                                                                              "Steamboat Montessori","Steamboat Mountain School","Strawberry Park",
                                                                              "Strawberry Park Elementary","Yampa Valley High School"))
SchoolCaseReportForm$School <- fct_collapse(SchoolCaseReportForm$School, "Soda Creek"=c("Soda Creek", "Soda Creek Elementary"),
                                                                        "Strawberry Park"=c("Strawberry Park", "Strawberry Park Elementary"))

#add columns to calculate iso dates
SchoolCaseReportForm <- SchoolCaseReportForm %>%
  mutate(INITIAL_LastDayIsolation = case_when(!is.na(SymptomOnset) ~ SymptomOnset+days(10),
                                      is.na(SymptomOnset) ~ TestDate+days(10)),
          INITIALFreeDate = INITIAL_LastDayIsolation+days(1),
         InfectiousStartDate = case_when(!is.na(SymptomOnset)&SymptomOnset<=TestDate ~ SymptomOnset-days(2),
                                         !is.na(SymptomOnset)&SymptomOnset>TestDate ~ TestDate-days(2),
                                         is.na(SymptomOnset) ~ TestDate-days(2)),
         LastUpdate = Sys.time()-hours(6), #subtracting 6 hours because google sheets always displays UTC and we want users to see "actual time"
         District = case_when(School %in% c("Sleeping Giant","Soda Creek", "Steamboat High","Steamboat Middle",
                                            "Strawberry Park","Yampa Valley High School") ~ "SSSD",
                              School %in% c("Hayden Elementary","Hayden Secondary") ~ "Hayden",
                              School %in% c("South Routt Elementary","SOROCO High","SOROCO Middle",
                                            "SOROCO Preschool at Yampa") ~ "SOROCO",
                              School=="North Routt Charter" ~ "NRCCS",
                              School=="Steamboat Montessori" ~ "Montessori",
                              School=="Steamboat Mountain School" ~ "Mountain School"))
SchoolCaseReportForm <- SchoolCaseReportForm %>%
  separate(Name,c("First Name", "Last Name"), remove = FALSE)

SchoolCaseReportForm$District <- factor(SchoolCaseReportForm$District, levels = c("SSSD", "Hayden", "SOROCO", "NRCCS", "Montessori", "Mountain School"))

#format dates as just date, not timestamp
#colnames(SchoolCaseReportForm)
SchoolCaseReportForm[,c(7,10,14,15,19:21)] <- lapply(SchoolCaseReportForm[,c(7,10,14,15,19:21)], as.Date.POSIXct, format = "%D")

#move contact person email
SchoolCaseReportForm <- SchoolCaseReportForm %>%
  relocate(ContactPersonEmail, .after=PrimaryContactPhoneNumber)

SchoolCaseReportForm <- SchoolCaseReportForm %>%
  mutate(RoleClean = case_when(Role %in% c("Teacher", "Staff", "Bus Driver") ~ "Teacher or Staff",
                               Role=="Student" ~ "Student",
                               TRUE ~ as.character("Other")))


# Join DrJ Data and Report Form -------------------------------------------

SchoolFormPostProcessing <- SchoolCaseReportForm %>%
  mutate(Name=str_to_lower(Name)) %>%
  left_join(DrJSchoolCases %>%
              filter(FinalDisposition=="reached_completed") %>%
              mutate(FullName = str_to_lower(paste0(FirstName," ",LastName))) %>%
              select(FullName,"RCPH_CICT_IsoEnd"=IsoEnd), by = c("Name" = "FullName")) %>%
  select(-Name) %>%
  relocate(RCPH_CICT_IsoEnd, .before=Timestamp)

#Add new tab to all responses spreadsheet with cleaned up data, new columns
sheet_write(SchoolFormPostProcessing, 
            ss = "https://docs.google.com/spreadsheets/d/1IS5XZL6i4xsnJEegykJYlik6BARRfetXbrQvpMYRd4g/edit?resourcekey#gid=164583539", 
            sheet = "All Cases Post Processing")

# Update individual school/district spreadsheets --------------------------


# SSSD Cases Google Sheets
# gs4_create(name = "SSSD Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="SSSD")) #this creates the sheet the first time
sheet_write(SchoolFormPostProcessing %>% filter(District=="SSSD"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "All Cases")  #this updates the existing sheet
sheet_write(SchoolFormPostProcessing %>% filter(School=="Sleeping Giant", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Sleeping Giant") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="Soda Creek", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Soda Creek") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="Steamboat High", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "High School") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="Steamboat Middle", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Middle School")
sheet_write(SchoolFormPostProcessing %>% filter(School=="Strawberry Park", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Strawberry Park")
sheet_write(SchoolFormPostProcessing %>% filter(School=="Yampa Valley High School", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Yampa Valley HS")


# Hayden Cases Google Sheets
# gs4_create(name = "Hayden Schools Cases Reported to Public Health", sheets = SchoolFormPostProcessing %>% filter(District=="Hayden")) #this creates the sheet the first time
sheet_write(SchoolFormPostProcessing %>% filter(District=="Hayden"), 
            ss = "https://docs.google.com/spreadsheets/d/1BZgy_2MLKOkAZgRM7Jc2c9WCZIkWVa8t6uRSLb-wb08/edit#gid=1989430456", 
            sheet = "All Cases")  #this updates the existing sheet
sheet_write(SchoolFormPostProcessing %>% filter(School=="Hayden Elementary", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/1BZgy_2MLKOkAZgRM7Jc2c9WCZIkWVa8t6uRSLb-wb08/edit#gid=1989430456", 
            sheet = "Hayden Elementary") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="Hayden Secondary", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/1BZgy_2MLKOkAZgRM7Jc2c9WCZIkWVa8t6uRSLb-wb08/edit#gid=1989430456", 
            sheet = "Hayden Secondary") 

# SOROCO Cases Google Sheets
# gs4_create(name = "SOROCO Schools Cases Reported to Public Health", sheets = SchoolFormPostProcessing %>% filter(District=="SOROCO")) #this creates the sheet the first time
sheet_write(SchoolFormPostProcessing %>% filter(District=="SOROCO"), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "All Cases")  #this updates the existing sheet
sheet_write(SchoolFormPostProcessing %>% filter(School=="South Routt Elementary", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "Elementary") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="SOROCO High", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "High") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="SOROCO Middle", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "Middle") 
sheet_write(SchoolFormPostProcessing %>% filter(School=="SOROCO Preschool at Yampa", Role %in% c("Teacher", "Staff", "Student", "Bus Driver", "Sports Participant")), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "Preschool") 

# NRCCS Cases Google Sheets
# gs4_create(name = "NRCCS Schools Cases Reported to Public Health", sheets = SchoolFormPostProcessing %>% filter(District=="NRCCS")) #this creates the sheet the first time
sheet_write(SchoolFormPostProcessing %>% filter(District=="NRCCS"), 
            ss = "https://docs.google.com/spreadsheets/d/1tVVsbr6v3YfqHtKjYK1OBqpQVnIQNqXsbBdrBuKk_E4/edit#gid=301098889", 
            sheet = "All Cases")  #this updates the existing sheet

# Montessori Cases Google Sheets
# gs4_create(name = "Montessori Schools Cases Reported to Public Health", sheets = SchoolFormPostProcessing %>% filter(District=="Montessori")) #this creates the sheet the first time
sheet_write(SchoolFormPostProcessing %>% filter(District=="Montessori"), 
            ss = "https://docs.google.com/spreadsheets/d/1Ehrl9vqbfLi9q75sk6fQNXlUPImbjBJ2hSj5qfgrtHA/edit#gid=1713156505", 
            sheet = "All Cases")  #this updates the existing sheet

# Mountain School Cases Google Sheets
# gs4_create(name = "Mountain School Schools Cases Reported to Public Health", sheets = SchoolFormPostProcessing %>% filter(District=="Mountain School")) #this creates the sheet the first time
sheet_write(SchoolFormPostProcessing %>% filter(District=="Mountain School"), 
            ss = "https://docs.google.com/spreadsheets/d/1-42rS7LkbzGP4C1R7db8Uiwf0TWC29SMOEGz3sDNXfw/edit#gid=1163719338", 
            sheet = "All Cases")  #this updates the existing sheet




