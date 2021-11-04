#Script to load Weekly Tracking Sheet data and prep for analysis
#First authored 10/15/2021 by Nicole Harty
#Last update: 10/15/2021

# Google Sheets Cases info and Visitors -----------------------------------

library(googlesheets4)
gs4_auth(email = "routtcodph@gmail.com")

# Satic old weeks'data (as of 10/15/2021, this is through week of Aug 16)
# StaticPHtracking <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
#                                sheet = 1, range = "A:M")
# for (i in 2:48) {
#   df <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
#                    sheet = i, range = "A:M")
#   df[,1:12] <- lapply(df[,1:12], as.character)
#   StaticPHtracking <- StaticPHtracking %>%
#     rbind(df)
# 
# }
# saveRDS(StaticPHtracking, file = "Data Files/StaticPHtracking.RDS")

StaticPHtracking <- readRDS("../Shared-Resources/COVID-Data-Files/StaticPHtracking.RDS")


nmaxGsheetTabs <- isoweek(Sys.Date())+15
PHtracking <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
                         sheet = 49, range = "A:M")
for (i in 50:nmaxGsheetTabs) {
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


#Visitors df
VisitorCases <- PHtracking %>%
  filter(`Resident or No`=="Visitor")


