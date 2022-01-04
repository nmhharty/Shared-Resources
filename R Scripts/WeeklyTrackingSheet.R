#Script to load Weekly Tracking Sheet data and prep for analysis
#First authored 10/15/2021 by Nicole Harty
#Last update: 12/17/2021

# Google Sheets Cases info and Visitors -----------------------------------

library(googlesheets4)
gs4_auth(email = "routtcodph@gmail.com")

# Satic old weeks' data (as of 12/17/2021 this is through Sept 26 2021)
# On 12/17/2021, exported all weeks from Weekly Tracking Google sheet through 9/26/2021 and removed from "Production" Weekly tracking. 
# Also deleted the columns that are no longer being used.
# StaticPHtracking <- read.xlsx("../Shared-Resources/COVID-Data-Files/Static Weekly Tracking.xlsx",
#                                sheet = 1, cols = c(1:13))
# for (i in 2:53) {
#   df <- read.xlsx("../Shared-Resources/COVID-Data-Files/Static Weekly Tracking.xlsx",
#                   sheet = i, cols = c(1:13))
#  # df[,1:12] <- lapply(df[,1:12], as.character)
#   StaticPHtracking <- StaticPHtracking %>%
#     rbind(df)
# 
# }
# StaticPHtracking <- StaticPHtracking %>%
#   select(1:2,5:8,10,11,13)
# colnames(StaticPHtracking)
# StaticPHtracking[,c(1,3)] <- lapply(StaticPHtracking[,c(1,3)], convertToDateTime)
# 
# colnames(StaticPHtracking) <-c("Date and Time of Notification","Patient Name","Specimen Collection Date","Resident or No",
#                                "Exposure Category","Contact Tracers Notified?","Is case in CEDRS?","Is case in Dr J?","Notes")
# 
# saveRDS(StaticPHtracking, file = "../Shared-Resources/COVID-Data-Files/StaticPHtracking.RDS")

StaticPHtracking <- readRDS("../Shared-Resources/COVID-Data-Files/StaticPHtracking.RDS")

nmaxGsheetTabs <- isoweek(Sys.Date())+14
PHtracking <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
                         sheet = 1, range = "A:I")
for (i in 2:nmaxGsheetTabs) {
  df <- range_read("https://docs.google.com/spreadsheets/d/1OZyEGlcBQqruCcNVO84lJv49s7S2uIqSGMyACbm0_DI/edit#gid=0",
                   sheet = i, range = "A:I")
  PHtracking <- PHtracking %>%
    rbind(df)
}

PHtracking <- PHtracking %>%
  rbind(StaticPHtracking)

PHtracking <- PHtracking %>%
  rename(ReportedDate = "Date and Time of Notification", Name = "Patient Name", Spec1_CollectionDate = "Specimen Collection Date",
         InCEDRS = "Is case in CEDRS?") %>%
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


