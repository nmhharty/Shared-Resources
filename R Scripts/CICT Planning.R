#Script to review case data to inform CICT prioritization
#First authored 3/1/2022 by Nicole Harty
#Last update: 4/12/2022
#


library(tidyverse)
library(lubridate)
library(openxlsx)

#UNCOMMENT BELOW LINES WHEN TROUBLESHOOTING. KEEP COMMENTED WHEN RUNNING PRODUCTION - PARAMETER IS SET IN MARKDOWN
DataThroughDate <- as.Date.character("2022-04-10")

RecentWeek <- interval(ymd(MostRecentWeekStart), ymd(MostRecentWeekStart+6))
Recent2Week <- interval(ymd(MostRecentWeekStart-7), ymd(MostRecentWeekStart+6))
RollingRecentWeek <- interval((DataThroughDate-6), DataThroughDate)
RollingRecent2Week <- interval((DataThroughDate-13), DataThroughDate)
RollingRecent2WeekComparison <- interval((DataThroughDate-20), (DataThroughDate-7))
RollingRecent1WeekComparison <- interval((DataThroughDate-13), (DataThroughDate-7))
RollingRecent4Week <- interval((DataThroughDate-27), DataThroughDate)

params <- list(DataThroughDate = "2022-04-09",
               MostRecentWeekStart = "2022-03-28",
               RouttPop = 25652,
               DataUpdateDate = "2022-04-11",
               Internal = FALSE)


#pull case data and internal tracking data
source("../Shared-Resources/R Scripts/TestResultsandCases.R")
source("../Shared-Resources/R Scripts/WeeklyTrackingSheet.R")


#median number cases per day, total and priority (<=18, >=70, hospitalized)
ConfProbCases %>%
  mutate("Day of Week" = wday(reporteddate, label = TRUE),
         Priority = case_when(age_at_reported<19|age_at_reported>69 ~ "Y",
                              hospitalized==TRUE ~ "Y",
                              TRUE ~ as.character("N"))) %>%
  filter(ReportedWeekStart>="2022-03-04") %>%
  group_by(ReportedWeekStart, `Day of Week`, Priority) %>%
  count() %>%
  plot_ly(x = ~`Day of Week`,
          y = ~n,
          type = 'box',
          color = ~Priority,
          #  boxpoints = "all",
          jitter = 0) %>%
  layout(title = "Number of Cases Reported Each Day, by Priotrity and Day of Week Since Mar 4",
         boxmode = "group",
         font = list(family = "Arial", size = 12))


