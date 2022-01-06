#Script to pull ShinyApps data for COVID dashboard
#First authored 1/3/2021 by Nicole Harty
#Last update: 1/6/2021


library(rsconnect)
library(RCurl)

#Pull connections data
#documentation on ShinyApps connections (like an impression or page view but not quite): https://docs.rstudio.com/shinyapps.io/metrics.html
Connections <- showMetrics(
  "container_status",
  "connect_count",
  appDir = "PUBLIC Dashboard",
  appName = "COVID19-Dashboard",
  from = as.numeric(as.POSIXct("2020-10-15")) #identifying date to begin data pull - this is when app went live
)

#AppUsage is most useful for authenticated apps -- doesn't help with public-facing apps to get proxy for "views"
AppUsage <- showUsage(
  appName = "COVID19-Dashboard",
  usageType = "hours",
  from = as.numeric(as.POSIXct("2020-10-15")), #identifying date to begin data pull - this is when app went live
  until = NULL,
  interval = NULL
)

AppUsage %>%
  mutate(Date = as.POSIXct(as.numeric(timestamp), origin = "1970-01-01")) %>%
  filter(Date >"2021-09-04") %>% #for some reason this app only has connections beginning 2021-09-07. Possible the data loss is related to the change in account email owner
  group_by(month(Date)) %>%
  summarise(mean(hours), max(hours), min(hours))


#rename columns because one has a blank column name
colnames(Connections) <- c("connect_count", "timestamp", "count")

#create summary of connections data as new connections per day
ConnectionSummary <- Connections %>%
  mutate(Date = as.POSIXct(as.numeric(timestamp), origin = "1970-01-01"),
         count = as.numeric(count),
         BeforeUsage = lag(count, order_by=timestamp),
         diff = count - BeforeUsage,
         Date = as.Date(Date),
         NewConnectCount = ifelse(diff<0,0,diff),
         NewConnectCount = ifelse(is.na(NewConnectCount),0,NewConnectCount)) %>%
  filter(Date>"2021-09-04") %>% #for some reason this app only has connections beginning 2021-09-07. Possible the data loss is related to the change in account email owner
  group_by(Date) %>%
  summarise(TotalConnect = sum(NewConnectCount)) %>%
  mutate(TotalConnect = ifelse(is.na(TotalConnect),0,TotalConnect))

#Check summary statistics on daily data
Connections %>% 
  mutate(Date = as.POSIXct(as.numeric(timestamp), origin = "1970-01-01"),
         count = as.numeric(count)) %>%
  filter(Date>"2021-09-04") %>% #for some reason this app only has connections beginning 2021-09-07. Possible the data loss is related to the change in account email owner
  group_by(month(Date)) %>%
  summarise(Mean = mean(count), max(count), min(count))

#check summary statistics
ConnectionSummary %>%
  filter(Date>"2021-09-06", Date<"2022-01-01") %>%
  group_by(month(Date)) %>%
  summarise(Mean = mean(TotalConnect), max(TotalConnect), min(TotalConnect))

#Plot of average new daily connections over time
ConnectionSummary %>%
  filter(Date>"2021-09-06", Date<"2022-01-01") %>%
  mutate(WeekStart = floor_date(Date, "week", week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(WeekStart) %>%
  summarise(MeanDaily = mean(TotalConnect)) %>%
  plot_ly(x = ~WeekStart,
          y = ~MeanDaily,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y",
          name = "Average Daily Connections, by Week") %>%
  layout(margin = list(l=50,r=50,h=175),
         xaxis = list(type = 'date',
                      tickformat = "%b %d, %Y",
                      autotick = F, 
                      tickangle = 45,
                      tick0 = "2000-01-10",
                      dtick = 1209600000,
                      title = "Week",
                      showgrid = FALSE),
         yaxis = list(title = "Average Daily New Connections",
                      showgrid = FALSE),
         legend = list(orientation = 'h', y=-.3),
         font = list(family = "Arial", size = 12),
         title = "Average Daily New Connections to Dashboard By Week")

#create data frame to run statistics on incidence vs daily sessions/connections
corr <- ConnectionSummary %>%
  filter(Date>"2021-09-13", Date<"2022-01-01") %>%
  mutate(WeekStart = floor_date(Date, "week", week_start = getOption("lubridate.week.start", 1))) %>%
  group_by(WeekStart) %>%
  summarise(MeanDaily = mean(TotalConnect)) %>%
  left_join(ConfProbCases %>%
              group_by(AttributionWeekStart) %>%
              summarise(NumberCases = n()) %>%
              right_join((Calendar %>%
                            select(WeekStart) %>%
                            distinct()), by = c("AttributionWeekStart" = "WeekStart")) %>%
              replace(., is.na(.), 0) %>%
              arrange(AttributionWeekStart) %>%
              filter(AttributionWeekStart<="2022-01-01", AttributionWeekStart>"2021-09-13") %>%
              mutate(TwoWeekCases=zoo::rollsumr(NumberCases, k = 2, fill = NA),
                     TwoWeekRate=(TwoWeekCases/25652*100000)), by = c("WeekStart" = "AttributionWeekStart"))

#correlation test of incidence vs average daily new connections/sessions
cor.test(x = corr$MeanDaily, y = corr$TwoWeekCases, method = "pearson")
#linear regression of incidence vs average daily new connections/sessions
lm(corr$MeanDaily ~ corr$TwoWeekCases) %>%
  summary()

#correlation scatter plot incidence vs average daily new connections/sessions
corr %>%
plot_ly(x = ~TwoWeekCases,
        y = ~MeanDaily,
        type = "scatter",
        color = I("#4E2B1F"),
        yaxis = "y",
        name = "Two-Week Incidence vs Average Daily Connections, by Week") %>%
  layout(margin = list(l=50,r=50,h=175),
         xaxis = list(title = "Two-Week Incidence (Count)",
                      showgrid = FALSE),
         yaxis = list(title = "Average Daily New Connections",
                      showgrid = FALSE),
         legend = list(orientation = 'h', y=-.3),
         font = list(family = "Arial", size = 12),
         title = "Average Daily New Connections to Dashboard By Week<br>(Since September 2021) Increases as Two-Week Incidence Increases")

#histograms to check normalcy of data
hist(corr$TwoWeekCases, breaks = c(0,100,200,300,400,500,600))
hist(corr$MeanDaily, breaks = c(0,40,80,120,160,200,240,280))
