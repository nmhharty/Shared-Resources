#Script to identify individuals in need of booster dose and generate reminder recall list
#First authored 3/2/2022 by Nicole Harty
#Last update: 3/9/2022


#identifying those due for booster and creating recall reminder list

#J&J Recipients
JJfirst <- PtIZ %>%
  filter(vaccination_code=="COVID-19 Vector-NR (JSN)"&dosage_num==1) %>%
  select(patient_id) %>%
  distinct() 

#adding booster info for all those with J&J first dose
JJbooster <- JJfirst %>%
  left_join(PtIZ %>%
              select(patient_id, vaccination_code, dosage_num, vaccination_date), by = "patient_id") %>%
  mutate(FirstDoseJJ = case_when(dosage_num==1&vaccination_code=="COVID-19 Vector-NR (JSN)" ~ "Y",
                                 TRUE ~ as.character("N")),
         Booster = case_when(dosage_num==2|dosage_num==3 ~ "Y",
                             # dosage_num==1&vaccination_code=="COVID-19 Vector-NR (JSN)" ~ "N",
                             dosage_num==1&vaccination_code=="COVID-19 mRNA (MOD)" ~ "Y",
                             dosage_num==1&vaccination_code=="COVID 12+yrs GRAY CAP" ~ "Y",
                             dosage_num==1&vaccination_code=="COVID 12+yrs PURPLE CAP" ~ "Y",
                             dosage_num==1&vaccination_code=="COVID-19 UF" ~ "Y"))

#Identifying those with only one dose and it's J&J
###THESE PEOPLE NEED BOOSTER
NeedJJbooster <- JJbooster %>%
  group_by(patient_id) %>%
  count() %>%
  filter(n==1) %>%
  select(patient_id)

#Moderna Recipients
##filter those who had J&J out of PtIZ list when identifying those with Moderna
##This will include individuals who received Pfizer perhaps before Moderna because the first time Moderna is delivered it's "dose 1"
MODfirst <- PtIZ %>%
  anti_join(JJfirst, by = "patient_id") %>%
  filter(vaccination_code=="COVID-19 mRNA (MOD)"&dosage_num==1) %>%
  select(patient_id) %>%
  distinct()

MODdoseCount <- MODfirst %>%
  left_join(PtIZ %>%
              select(patient_id, vaccination_code, dosage_num, vaccination_date, FullyVax), by = "patient_id") %>%
  filter(FullyVax=="Yes") %>% #excluding those who are not fully vaccinated because they haven't even gotten second dose, so that's a different issue
  select(patient_id) %>%
  distinct() %>%
  left_join(PtIZ %>%
              select(patient_id, vaccination_code, dosage_num, vaccination_date), by = "patient_id") %>%
  group_by(patient_id) %>%
  count()

NeedMODbooster <- MODdoseCount %>%
  filter(n<3)
  
  
  
# #Pfizer Recipients
##filter those who had J&J AND those address in MOD identification  out of PtIZ list when identifying those with Moderna
##This will include individuals who received Pfizer perhaps before Moderna because the first time Moderna is delivered it's "dose 1"
##This only is for those currently eligible for boosters, so 12+
PFZfirst <- PtIZ %>%
  anti_join(JJfirst, by = "patient_id") %>%
  anti_join(MODfirst, by = "patient_id") %>%
  filter((vaccination_code=="COVID 12+yrs PURPLE CAP"&dosage_num==1)|(vaccination_code=="COVID 12+ GRAY CAP"&dosage_num==1)) %>%
  select(patient_id) %>%
  distinct()

PFZdoseCount <- PFZfirst %>%
  left_join(PtIZ %>%
              select(patient_id, vaccination_code, dosage_num, vaccination_date, FullyVax), by = "patient_id") %>%
  filter(FullyVax=="Yes") %>% #excluding those who are not fully vaccinated because they haven't even gotten second dose, so that's a different issue
  select(patient_id) %>%
  distinct() %>%
  left_join(PtIZ %>%
              select(patient_id, vaccination_code, dosage_num, vaccination_date), by = "patient_id") %>%
  group_by(patient_id) %>%
  count()

NeedPFZbooster <- PFZdoseCount %>%
  filter(n<3)


#Pull together details on everyone who needs booster: NeedJJbooster, NeedMODbooster, NeedPFZbooster

#will call everyone with only one J&J dose
JJboosterCallList <- NeedJJbooster %>%
  select(patient_id) %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob, age_at_1stvaccination, patient_telephone, patient_email, vaccination_code, dosage_num, 
                     vaccination_date, clinic_desc), by = "patient_id") %>%
  filter(age_at_1stvaccination>65) %>%
  select(-age_at_1stvaccination, -dosage_num) %>%
  mutate(WeeksSinceLastDose = as.numeric(difftime(as.Date("2022-03-01"),as.Date(vaccination_date), units = c("weeks")))) %>%
  filter(WeeksSinceLastDose>10)

#65+ for Moderna
MODboosterCallList <- NeedMODbooster %>%
  select(patient_id) %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob, age_at_1stvaccination, patient_telephone, patient_email, vaccination_code, dosage_num, 
                     vaccination_date, clinic_desc), by = "patient_id") %>%
  filter(dosage_num=="2"&age_at_1stvaccination>65) %>%
  select(-age_at_1stvaccination, -dosage_num) %>%
  mutate(WeeksSinceLastDose = as.numeric(difftime(as.Date("2022-03-01"),as.Date(vaccination_date), units = c("weeks")))) %>%
  filter(WeeksSinceLastDose>22)

#65+ for Pfizer
PFZboosterCallList <- NeedPFZbooster %>% 
  select(patient_id) %>%
  left_join(PtIZ %>%
              select(patient_id, patient_first_name, patient_last_name, patient_dob, age_at_1stvaccination, patient_telephone, patient_email, vaccination_code, dosage_num, 
                     vaccination_date, clinic_desc), by = "patient_id") %>%
  filter(dosage_num=="2"&age_at_1stvaccination>65) %>%
  select(-age_at_1stvaccination, -dosage_num) %>%
  mutate(WeeksSinceLastDose = as.numeric(difftime(as.Date("2022-03-01"),as.Date(vaccination_date), units = c("weeks")))) %>%
  filter(WeeksSinceLastDose>22)

#create Google Sheet with call details, one tab for people who received each type of vaccine
gs4_create("Booster Reminder Recall", sheets = list(JJinitial = JJboosterCallList, MODinitial = MODboosterCallList, PFZinitial = PFZboosterCallList))


