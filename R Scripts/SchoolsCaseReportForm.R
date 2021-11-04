#Script to load Schools Case Report data for analysis
#First authored 10/6/2021 by Nicole Harty
#Last update: 10/6/2021

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

# School COVID Case Report Form Data Wrangling ----------------------------
#load data from Google Spreadsheet
gs4_auth(email = "routtcodph@gmail.com")
SchoolCaseReportForm <- range_read("https://docs.google.com/spreadsheets/d/1IS5XZL6i4xsnJEegykJYlik6BARRfetXbrQvpMYRd4g/edit?resourcekey#gid=164583539") %>%
  filter(Timestamp>"2021-10-08")

#clean up column names
colnames(SchoolCaseReportForm) <- c("Timestamp","Email","School","Name", "DOB","PrimaryContact","PrimaryContactPhoneNumber", "TestDate", "TestType",
                                    "Role","Grade","LastDaySchool","SymptomOnset","Extracurriculars","Notes")

SchoolCaseReportForm$School <- factor(SchoolCaseReportForm$School, levels = c("Hayden Elementary","Hayden Secondary","North Routt Charter","Sleeping Giant",
                                                                              "Soda Creek","South Routt Elementary","SOROCO High","SOROCO Middle",
                                                                              "SOROCO Preschool at Yampa","Steamboat High","Steamboat Middle",
                                                                              "Steamboat Montessori","Steamboat Mountain School","Strawberry Park",
                                                                              "Yampa Valley High School"))

#add columns to calculate iso dates
SchoolCaseReportForm <- SchoolCaseReportForm %>%
  mutate(LastDayIsolation = case_when(!is.na(SymptomOnset) ~ SymptomOnset+days(10),
                                      is.na(SymptomOnset) ~ TestDate+days(10)),
         FreeDate = LastDayIsolation+days(1),
         InfectiousStartDate = case_when(!is.na(SymptomOnset)&SymptomOnset<=TestDate ~ SymptomOnset-days(2),
                                         !is.na(SymptomOnset)&SymptomOnset>TestDate ~ TestDate-days(2),
                                         is.na(SymptomOnset) ~ TestDate-days(2)),
         LastUpdate = Sys.time()-hours(6), #subtracting 6 hours because google sheets always displays UTC and we want users to see "actual time"
         District = case_when(School %in% c("Sleeping Giant","Soda Creek","Steamboat High","Steamboat Middle",
                                            "Strawberry Park","Yampa Valley High School") ~ "SSSD",
                              School %in% c("Hayden Elementary","Hayden Secondary") ~ "Hayden",
                              School %in% c("South Routt Elementary","SOROCO High","SOROCO Middle",
                                            "SOROCO Preschool at Yampa") ~ "SOROCO",
                              School=="North Routt Charter" ~ "NRCCS",
                              School=="Steamboat Montessori" ~ "Montessori",
                              School=="Steamboat Mountain School" ~ "Mountain School"))

SchoolCaseReportForm$District <- factor(SchoolCaseReportForm$District, levels = c("SSSD", "Hayden", "SOROCO", "NRCCS", "Montessori", "Mountain School"))

#format dates as just date, not timestamp
colnames(SchoolCaseReportForm)
SchoolCaseReportForm[,c(5,8,12:13, 16:18)] <- lapply(SchoolCaseReportForm[,c(5,8,12:13, 16:18)], as.Date.POSIXct, format = "%D")

#Add new tab to all responses spreadsheet with cleaned up data, new columns
sheet_write(SchoolCaseReportForm, 
            ss = "https://docs.google.com/spreadsheets/d/1IS5XZL6i4xsnJEegykJYlik6BARRfetXbrQvpMYRd4g/edit?resourcekey#gid=164583539", 
            sheet = "All Cases Post Processing")

# Update individual school/district spreadsheets --------------------------


# SSSD Cases Google Sheets
# gs4_create(name = "SSSD Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="SSSD")) #this creates the sheet the first time
sheet_write(SchoolCaseReportForm %>% filter(District=="SSSD"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "All Cases")  #this updates the existing sheet
sheet_write(SchoolCaseReportForm %>% filter(School=="Sleeping Giant"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Sleeping Giant") 
sheet_write(SchoolCaseReportForm %>% filter(School=="Soda Creek"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Soda Creek") 
sheet_write(SchoolCaseReportForm %>% filter(School=="Steamboat High"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "High School") 
sheet_write(SchoolCaseReportForm %>% filter(School=="Steamboat Middle"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Middle School")
sheet_write(SchoolCaseReportForm %>% filter(School=="Strawberry Park"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Strawberry Park")
sheet_write(SchoolCaseReportForm %>% filter(School=="Yampa Valley High School"), 
            ss = "https://docs.google.com/spreadsheets/d/13BngoXXlYP7vxHvZGQAV6XZWFYO_ovpsOUO8U7MxkqM/edit", 
            sheet = "Yampa Valley HS")

# Hayden Cases Google Sheets
# gs4_create(name = "Hayden Schools Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="Hayden")) #this creates the sheet the first time
sheet_write(SchoolCaseReportForm %>% filter(District=="Hayden"), 
            ss = "https://docs.google.com/spreadsheets/d/1BZgy_2MLKOkAZgRM7Jc2c9WCZIkWVa8t6uRSLb-wb08/edit#gid=1989430456", 
            sheet = "All Cases")  #this updates the existing sheet
sheet_write(SchoolCaseReportForm %>% filter(School=="Hayden Elementary"), 
            ss = "https://docs.google.com/spreadsheets/d/1BZgy_2MLKOkAZgRM7Jc2c9WCZIkWVa8t6uRSLb-wb08/edit#gid=1989430456", 
            sheet = "Hayden Elementary") 
sheet_write(SchoolCaseReportForm %>% filter(School=="Hayden Secondary"), 
            ss = "https://docs.google.com/spreadsheets/d/1BZgy_2MLKOkAZgRM7Jc2c9WCZIkWVa8t6uRSLb-wb08/edit#gid=1989430456", 
            sheet = "Hayden Secondary") 

# SOROCO Cases Google Sheets
# gs4_create(name = "SOROCO Schools Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="SOROCO")) #this creates the sheet the first time
sheet_write(SchoolCaseReportForm %>% filter(District=="SOROCO"), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "All Cases")  #this updates the existing sheet
sheet_write(SchoolCaseReportForm %>% filter(School=="South Routt Elementary"), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "Elementary") 
sheet_write(SchoolCaseReportForm %>% filter(School=="SOROCO High"), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "High") 
sheet_write(SchoolCaseReportForm %>% filter(School=="SOROCO Preschool at Yampa"), 
            ss = "https://docs.google.com/spreadsheets/d/1ZZVAeT6AModserUl83daYh3RkIoFAoOUcIUgwp8SbjA/edit#gid=871158826", 
            sheet = "Preschool") 

# NRCCS Cases Google Sheets
# gs4_create(name = "NRCCS Schools Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="NRCCS")) #this creates the sheet the first time
sheet_write(SchoolCaseReportForm %>% filter(District=="NRCCS"), 
            ss = "https://docs.google.com/spreadsheets/d/1tVVsbr6v3YfqHtKjYK1OBqpQVnIQNqXsbBdrBuKk_E4/edit#gid=301098889", 
            sheet = "All Cases")  #this updates the existing sheet

# Montessori Cases Google Sheets
# gs4_create(name = "Montessori Schools Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="Montessori")) #this creates the sheet the first time
sheet_write(SchoolCaseReportForm %>% filter(District=="Montessori"), 
            ss = "https://docs.google.com/spreadsheets/d/1Ehrl9vqbfLi9q75sk6fQNXlUPImbjBJ2hSj5qfgrtHA/edit#gid=1713156505", 
            sheet = "All Cases")  #this updates the existing sheet

# Mountain School Cases Google Sheets
# gs4_create(name = "Mountain School Schools Cases Reported to Public Health", sheets = SchoolCaseReportForm %>% filter(District=="Mountain School")) #this creates the sheet the first time
sheet_write(SchoolCaseReportForm %>% filter(District=="Mountain School"), 
            ss = "https://docs.google.com/spreadsheets/d/1-42rS7LkbzGP4C1R7db8Uiwf0TWC29SMOEGz3sDNXfw/edit#gid=1163719338", 
            sheet = "All Cases")  #this updates the existing sheet




