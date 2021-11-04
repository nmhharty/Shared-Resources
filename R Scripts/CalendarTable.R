#Script to create Calendar Table
#First authored 10/18/2021 by Nicole Harty
#Last update: 10/18/2021

Calendar <- data.frame(date = seq(as.Date("2020/03/01"), as.Date(Sys.Date()+30), "days")) %>%
  mutate(Year = year(date),
         Week = isoweek(date),
         WeekStart = floor_date(date, "week", week_start = getOption("lubridate.week.start", 1)),
         WeekEnd = ceiling_date(date, "week", week_start = getOption("lubridate.week.start", 7),change_on_boundary = FALSE),
  )
