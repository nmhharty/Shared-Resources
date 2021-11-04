#Script to load symptom info from Dr J database and prep for analysis
#First authored 10/15/2021 by Nicole Harty
#Last update: 10/15/2021


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
              select(eventid, profileid, casestatus, gender, date_of_birth,
                     onsetdate, earliest_collectiondate, AttributionDate, reporteddate, AttributionWeekStart), by = c("event_id" = "eventid")) %>%
  mutate(SymptomsBeforeTest = case_when(symptom_onset_date<=AttributionDate ~ "Yes",
                                        TRUE ~ as.character("No")))


##THIS IS FOR USING CEDRS
SymptomOverview <- ConfProbCases %>%
  select(eventid, profileid, casestatus, gender, date_of_birth,
         onsetdate, earliest_collectiondate, AttributionDate, reporteddate, AttributionWeekStart) %>%
  mutate(SymptomsBeforeTest = case_when(onsetdate<=AttributionDate ~ "Yes",
                                        TRUE ~ as.character("No")),
         SymptomsEver = case_when(is.na(onsetdate) ~ "No",
                                  TRUE ~ as.character("Yes")))
