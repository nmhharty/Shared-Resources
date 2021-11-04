#Script to load Hospitalization info from Routt hospitalization tracking and COPHS, prep for analysis
#First authored 10/15/2021 by Nicole Harty
#Last update: 10/15/2021


# Hospitalizations --------------------------------------------------------

Hospit <- read.xlsx("../Shared-Resources/COVID-Data-Files/HospitalizationData.xlsx")
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
COPHS <- read.delim("../Shared-Resources/COVID-Data-Files/cophs_Routt.txt", sep = "|") %>%
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


