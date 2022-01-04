#Script to determine school-specific vaccination rate
#First authored 8/9/2021 by Nicole Harty
#Last update: 12/3/2021


library(openxlsx)
library(tidyverse)
library(lubridate)

#load PtIZ
PtIZ <- read.delim("..Shared-Resources/COVID-Data-Files/PatientImmunizations_Routt.txt", sep = "|", header = TRUE#, fileEncoding = "utf16"
)

colnames(PtIZ)
PtIZ <- PtIZ %>%
  mutate(AgeGroup10yr = cut(age_at_1stvaccination, breaks=c(0,9,19,29,39,49,59,69,79,89,130), 
                            labels=c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90 and over")),
         AgeGroup5yr = cut(age_at_1stvaccination, breaks=c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74,79,84,89,94,130), 
                           labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59",
                                    "60-64","65-69","70-74","75-79","80-84","85-89","90-94","95 and over")))
PtIZ[,c(4, 9:10)] <- lapply(PtIZ[,c(4, 9:10)], as.Date.character, "%m/%d/%Y")


# SSSD Load data -------------------------------

#load school rosters
SGS <- read.xlsx("..Shared-Resources/COVID-Data-Files/SSSD Vax Info/SGS COVID vaccine data 9-13-21.xlsx") %>%
  select(student_lastName, student_firstName, student_birthdate)
SSHS <- read.xlsx("..Shared-Resources/COVID-Data-Files/SSSD Vax Info/SSHS COVID vaccine data 9-13-21.xlsx") %>%
  select(student_lastName, student_firstName, student_birthdate)
SSMS <- read.xlsx("..Shared-Resources/COVID-Data-Files/SSSD Vax Info/SSMS COVID vaccine data 9-13-21.xlsx") %>%
  select(student_lastName, student_firstName, student_birthdate)
YVHS <- read.xlsx("..Shared-Resources/COVID-Data-Files/SSSD Vax Info/YVHS COVID vaccine data 9-13-21.xlsx") %>%
  select(student_lastName, student_firstName, student_birthdate)

SGS$student_birthdate <- as.Date.character(SGS$student_birthdate, "%m/%d/%Y")
SSHS$student_birthdate <- as.Date.character(SSHS$student_birthdate, "%m/%d/%Y")
SSMS$student_birthdate <- as.Date.character(SSMS$student_birthdate, "%m/%d/%Y")
YVHS$student_birthdate <- as.Date.character(YVHS$student_birthdate, "%m/%d/%Y")

#convert names to all caps for joins
SGS[,c(1:2)] <- lapply(SGS[,c(1:2)], toupper)
SSHS[,c(1:2)] <- lapply(SSHS[,c(1:2)], toupper)
SSMS[,c(1:2)] <- lapply(SSMS[,c(1:2)], toupper)
YVHS[,c(1:2)] <- lapply(YVHS[,c(1:2)], toupper)



# SSSD Join IZ and Rosters -----------------------------------------------------

SGS_IZ <- SGS %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("student_lastName" = "patient_last_name", 
                                 "student_firstName" = "patient_first_name", "student_birthdate" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))

SSHS_IZ <- SSHS %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("student_lastName" = "patient_last_name", 
                                 "student_firstName" = "patient_first_name", "student_birthdate" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))

SSMS_IZ <- SSMS %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("student_lastName" = "patient_last_name", 
                                 "student_firstName" = "patient_first_name", "student_birthdate" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))

YVHS_IZ <- YVHS %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("student_lastName" = "patient_last_name", 
                                 "student_firstName" = "patient_first_name", "student_birthdate" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))



# SSSD Vaccine Rates --------------------------------------------------------

(SGS_IZ %>%
  summarise(sum(VaxDose))) / (SGS_IZ %>% count())

(SSHS_IZ %>%
    summarise(sum(VaxDose))) / (SSHS_IZ %>% count())

(SSMS_IZ %>%
    summarise(sum(VaxDose))) / (SSMS_IZ %>% count())

(YVHS_IZ %>%
    summarise(sum(VaxDose))) / (YVHS_IZ %>% count())



# Hayden Load data --------------------------------

#load school rosters
Hayden6 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 1, colNames = FALSE)
Hayden7 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 2, colNames = FALSE)
Hayden8 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 3, colNames = FALSE)
Hayden9 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 4, colNames = FALSE)
Hayden10 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 5, colNames = FALSE)
Hayden11 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 6, colNames = FALSE)
Hayden12 <- read.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/Hayden Rosters 6-12.xlsx", sheet = 7, colNames = FALSE)

HaydenStudents <- Hayden6 %>%
  rbind(Hayden7) %>%
  rbind(Hayden8) %>%
  rbind(Hayden9) %>%
  rbind(Hayden10) %>%
  rbind(Hayden11) %>%
  rbind(Hayden12) 
colnames(HaydenStudents) <- c("LastName", "FirstName", "DOB")


HaydenStudents$DOB <- as.Date.numeric(HaydenStudents$DOB, origin = "1899-12-30")


#convert names to all caps for joins
HaydenStudents[,c(1:2)] <- lapply(HaydenStudents[,c(1:2)], toupper)



# Hayden Join IZ and Rosters -----------------------------------------------------

HaydenStudents_IZ <- HaydenStudents %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("LastName" = "patient_last_name", 
                                 "FirstName" = "patient_first_name", 
                                 "DOB" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))



# Hayden Vaccine Rates --------------------------------------------------------

(HaydenStudents_IZ %>%
   summarise(sum(VaxDose))) / (HaydenStudents_IZ %>% count())

HaydenVaxSummary <- data.frame(StudentPop = HaydenStudents_IZ %>% count() %>% pull(),
           StudentVaxPop = ((HaydenStudents_IZ %>%
                              summarise(sum(VaxDose))) / (HaydenStudents_IZ %>% count())),
           AdditionalNeeded = ((.8)*(HaydenStudents_IZ %>% count() %>% pull())-(HaydenStudents_IZ %>%
                                                                           summarise(sum(VaxDose)) %>% pull())))
HaydenVaxSummary %>%
  mutate(StudentVaXPop = scales::percent(sum.VaxDose.)) %>%
  write.xlsx("..Shared-Resources/COVID-Data-Files/Hayden Vax Info/HaydenVaxSummary.xlsx")



# Montessori Load Roster --------------------------------------------------
MontessoriStudents <- read.xlsx("T:/COVID R Files/Shared-Resources/COVID-Data-Files/Montessori Vax Info/Steamboat Montessori K-6th Roster.xlsx")
MontessoriStudents$DOB <- as.Date(MontessoriStudents$DOB, origin = "1899-12-30")

MontessoriStudents[,c(2:3)] <- lapply(MontessoriStudents[,c(2:3)], toupper)


# Montessori Join IZ and Rosters -----------------------------------------------------

MontessoriStudents_IZ <- MontessoriStudents %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("LAST.NAME" = "patient_last_name", 
                                 "STUDENT.NAME" = "patient_first_name", 
                                 "DOB" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))



# Montessori Vaccine Rates ------------------------------------------------

(MontessoriStudents_IZ %>%
   summarise(sum(VaxDose))) / (MontessoriStudents_IZ %>% count())

MontessoriVaxSummary <- data.frame(StudentPop = MontessoriStudents_IZ %>% count() %>% pull(),
                                   StudentVaxPop = (MontessoriStudents_IZ %>%
                                                   summarise(sum(VaxDose))) / (MontessoriStudents_IZ %>% count()),
                                   AdditionalNeeded = ((.8)*(MontessoriStudents_IZ %>% count() %>% pull())-(MontessoriStudents_IZ %>%
                                                                                                      summarise(sum(VaxDose)) %>% pull())))
MontessoriVaxSummary %>%
  mutate(StudentVaXPop = scales::percent(sum.VaxDose.)) %>%
  write.xlsx("T:/COVID R Files/Shared-Resources/COVID-Data-Files/Montessori Vax Info/MontessoriVaxSummary.xlsx")


# Mountain School Load Roster --------------------------------------------------
MtnSchoolStudents <- read.xlsx("T:/COVID R Files/Shared-Resources/COVID-Data-Files/SMS Vax Info/SMS.K_8.xlsx")
MtnSchoolStudents$birthdate <- as.Date(MtnSchoolStudents$birthdate, origin = "1899-12-30")

MtnSchoolStudents[,c(1:2)] <- lapply(MtnSchoolStudents[,c(1:2)], toupper)


# Mountain School Join IZ and Rosters -----------------------------------------------------

MtnSchoolStudents_IZ <- MtnSchoolStudents %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob) %>%
              distinct(), by = c("last_name" = "patient_last_name", 
                                 "first_name" = "patient_first_name", 
                                 "birthdate" = "patient_dob")) %>%
  mutate(VaxDose = case_when(is.na(patient_id) ~ 0,
                             !is.na(patient_id) ~ 1))



# Mountain School Vaccine Rates ------------------------------------------------

(MtnSchoolStudents_IZ %>%
   summarise(sum(VaxDose))) / (MtnSchoolStudents_IZ %>% count())

MtnSchoolVaxSummary <- data.frame(StudentPop = MtnSchoolStudents_IZ %>% count() %>% pull(),
                                   StudentVaxPop = (MtnSchoolStudents_IZ %>%
                                                      summarise(sum(VaxDose))) / (MtnSchoolStudents_IZ %>% count()),
                                   AdditionalNeeded = ((.8)*(MtnSchoolStudents_IZ %>% count() %>% pull())-(MtnSchoolStudents_IZ %>%
                                                                                                              summarise(sum(VaxDose)) %>% pull())))
MtnSchoolVaxSummary %>%
  mutate(StudentVaXPop = scales::percent(sum.VaxDose.)) %>%
  write.xlsx("T:/COVID R Files/Shared-Resources/COVID-Data-Files/SMS Vax Info/MtnSchoolVaxSummary.xlsx")




