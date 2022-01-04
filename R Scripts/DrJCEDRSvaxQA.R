#Script to QA vaccination data in CEDRS/Dr J
#First authored 12/1/2021 by Nicole Harty
#Last update: 12/1/2021


# Identify CEDRS cases with no vax data -----------------------------------
source("../Shared-Resources/R Scripts/TestResultsandCases.R")
CEDRSnoVax <- ConfProbCases %>%
  filter(is.na(vax_firstdose) & AttributionDate>"2021-01-30") %>%
  select(eventid, reporteddate, first_name, last_name, date_of_birth, address1) %>%
  mutate(first_name = str_to_lower(first_name),
         last_name = str_to_lower(last_name))


# Pull Vax Data from Dr J -------------------------------------------------

library(DBI)
library(odbc)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server=s-vm10-ph01;database=JUSTINA", timeout = 10)
#DrJ dataframe for joining to CEDRS
DrJdata <- dbGetQuery(con, "SELECT CAST(date_opened as Date) as DateOpened, CAST(event_id as int) as eventid, CAST(first_name as varchar(256)) as first_name,
                              CAST(last_name as varchar(256)) as last_name, CAST(dob as Date) as DOB, CAST(address as varchar(256)) as Address,
                              CAST(phone_home as varchar(256)) as Phone1, CAST(phone_work as varchar(256)) as Phone2, received_vaccine
                        FROM JUSTINA.dbo.patient")
DrJdata %>%
  mutate(first_name = str_to_lower(first_name),
         last_name = str_to_lower(last_name))


# Join data for list of cases with CEDRS vax + Dr J vax -------------------

#CEDRS case details for those who indicate received vaccine in Dr J with phone number info in Dr J
DrJvaxYES <- CEDRSnoVax %>%
  inner_join(DrJdata %>%
              select(-first_name, -last_name) %>%
              filter(received_vaccine=="yes"), by = "eventid")

#CEDRS case details for everyone else + phone number info from Dr J

CEDRSnoVax <- CEDRSnoVax %>%
  left_join(DrJdata %>%
              select(eventid, Phone1, Phone2, received_vaccine), by = "eventid")

CEDRSnoVax <- CEDRSnoVax %>%
  mutate(received_vaccine = case_when(received_vaccine=="yes" ~ "yes",
                                      received_vaccine=="no" ~ "no",
                                      TRUE ~ "unknown or missing"))

CEDRSnoVaxnoDrJ <- CEDRSnoVax %>%
  anti_join(DrJvaxYES %>%
              select(eventid), by = "eventid") %>% 
  arrange(desc(received_vaccine)) %>%
  distinct()

# Publish to Google Sheets for review -------------------------------------

#create main worksheet
# gs4_create(name = "Cases Vaccination QA", sheets = DrJvaxYES %>% arrange(reporteddate)) #this creates the sheet the first time


#Add sheets

# range_write(ss = "https://docs.google.com/spreadsheets/d/1VzJITfMUFFHu0ZQQG_CgOnlpt-ywgsaTQvoXOfPUBEk/edit#gid=716749094",
#             DrJvaxYES %>%
#               arrange(desc(reporteddate)),
#             sheet = "DrJyesVax")

sheet_write(CEDRSnoVaxnoDrJ %>%
              filter(received_vaccine=="unknown or missing") %>%
              arrange(desc(reporteddate)), 
            ss = "https://docs.google.com/spreadsheets/d/1VzJITfMUFFHu0ZQQG_CgOnlpt-ywgsaTQvoXOfPUBEk/edit#gid=716749094", 
            sheet = "DrJMissingUnknownVax")

sheet_write(CEDRSnoVaxnoDrJ %>%
              filter(received_vaccine=="no") %>%
              arrange(desc(reporteddate)), 
            ss = "https://docs.google.com/spreadsheets/d/1VzJITfMUFFHu0ZQQG_CgOnlpt-ywgsaTQvoXOfPUBEk/edit#gid=716749094", 
            sheet = "DrJvaxNo")

