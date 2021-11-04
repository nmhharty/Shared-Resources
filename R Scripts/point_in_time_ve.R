#Script to replicate CDPHE's point in time vaccine efficacy, code primarily written by CDPHE
#First authored 7/22/2021 by Nicole Harty
#Last update: 8/26/2021

#8/26/2021: Cases df from LPHA portal now includes breakthrough, partialonly, and vaccine_received columns so don't need to create breakthrough column with join

library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(dbplyr)
library(ggplot2)
library(janitor)
library(purrr)
library(plotly)


#pull Dr J data on vaccinations to add in people who have CIIS data but for another address ??

#Load data files
IZ <- read.delim("PatientImmunizations_Routt.txt", sep = "|", header = TRUE#, fileEncoding = "utf16"
)
IZ[,c(4, 9:10)] <- lapply(IZ[,c(4, 9:10)], as.Date.character, "%m/%d/%Y")
IZ <- IZ %>% 
  mutate(FullyVax = case_when(vaccination_code=="COVID-19 Vector-NR (JSN)" ~ "Yes",
                              vaccination_code!="COVID-19 Vector-NR (JSN)"&dosage_num==2 ~ "Yes",
                              TRUE ~ as.character("No"))) 

Cases <- read.delim("cedrs_Routt.txt", sep = "|", header = TRUE#, fileEncoding = "utf16"
)
Cases[c(7,10:14,16)] <- lapply(Cases[c(7,10:14,16)], as.Date.character)

#Create Calendar table
Calendar <- data.frame(date = seq(as.Date("2020/03/01"), as.Date(Sys.Date()+30), "days")) %>%
  mutate(Year = year(date),
         Week = isoweek(date),
         WeekStart = floor_date(date, "week", week_start = getOption("lubridate.week.start", 1)),
         WeekEnd = ceiling_date(date, "week", week_start = getOption("lubridate.week.start", 7),change_on_boundary = FALSE),
  )


#VaxCases is listing of cases who are fully vaccinated (>14 days from last dose)
VaxCases <- Cases %>%
  select(eventid, first_name, last_name, date_of_birth, reporteddate, collectiondate, onsetdate_proxy_dist, breakthrough) %>%
  mutate(attribution_date = case_when(is.na(collectiondate) ~ reporteddate,
                                      TRUE ~ collectiondate))
VaxCases <- VaxCases %>%
  inner_join((IZ %>%
                select(patient_id, patient_first_name, patient_last_name, patient_dob, vaccination_date, vaccination_code, FullyVax) %>%
                filter(FullyVax=="Yes")),
             by = c("first_name" = "patient_first_name", "last_name" = "patient_last_name", "date_of_birth" = "patient_dob")) %>%
  # filter(AttributionDate>(vaccination_date+14)) %>%
  mutate(DaysVaxMinusTest = as.numeric(difftime(attribution_date,vaccination_date, units = "days"))) %>%
  rename(vax_utd = vaccination_date)

#Add details on first and last vax, vax breakthough, and dose info to all cases
Cases <- Cases %>%
  left_join(IZ %>% 
              group_by(patient_id) %>% 
              filter(vaccination_date == min(vaccination_date)) %>% 
              select(patient_id, patient_first_name, patient_last_name, patient_dob, vaccination_date) %>% 
              distinct() %>% 
              ungroup() %>% 
              rename(first_vax = vaccination_date), by = c("first_name" = "patient_first_name", "last_name" = "patient_last_name", "date_of_birth" = "patient_dob")) %>%
  left_join(VaxCases %>%
              select(eventid, vaccination_code, vax_utd, DaysVaxMinusTest, breakthrough), by = "eventid") 
# %>%
#   mutate(breakthrough = case_when(breakthrough==1 ~ 1,
#                                   TRUE ~ 0))

cases2 <- Cases %>% 
  mutate(vax_utd14 = vax_utd + days(14), 
         partial = DaysVaxMinusTest >= 0 & !breakthrough) %>% 
  select(profileid, eventid, reporteddate, breakthrough, partial) %>% 
  mutate(partial = ifelse(is.na(partial), F, partial))


weeksall <- Calendar %>% 
  rename(start = WeekStart, end = WeekEnd) %>%
  group_by(start) %>% 
  mutate(n = n()) %>% 
  filter(n == 7) %>% 
  select(-n) %>% 
  ungroup() %>% 
  select(date, start, end)

casesbyweek <- cases2 %>% 
  left_join(weeksall, by = c("reporteddate" = "date")) %>% 
  filter(!is.na(start), 
         !partial) %>% 
  group_by(breakthrough) %>% 
  count(start) %>% 
  arrange(start, breakthrough) %>% 
  ungroup() %>% 
  mutate(breakthrough = ifelse(breakthrough, "breakthrough", "unvaccinated")) %>% 
  spread(breakthrough, n) %>% 
  replace(.,is.na(.),0) %>%
  arrange(start) 


IZlist <- IZ %>%
  select(patient_id, patient_first_name, patient_last_name, patient_dob, vaccination_date, vaccination_code) %>%
  group_by(patient_id) %>%
  slice(which.min(vaccination_date)) %>%
  mutate(UTD_on = case_when(vaccination_code=="COVID-19 Vector-NR (JSN)" ~ vaccination_date,
                            vaccination_code=="COVID-19 mRNA (PFR)" ~ vaccination_date+days(21),
                            vaccination_code=="COVID-19 mRNA (MOD)" ~ vaccination_date+days(28))) %>%
  ungroup() %>%
  rename(first_vacc_date = vaccination_date)

utd <- IZlist %>% 
  select(patient_id, UTD_on, first_vacc_date) %>% 
  mutate(utd = ymd(str_sub(UTD_on + days(14), 1, 10)), 
         first_vacc_date = ymd(str_sub(first_vacc_date, 1, 10))) %>% 
  select(patient_id, utd, first_vacc_date) 

utd2 <- utd %>% 
  left_join(weeksall %>% select(-end) %>% rename(utd_week = start), 
            by = c("utd" = "date")) %>% 
  left_join(weeksall %>% select(-end) %>% rename(firstvac_week = start), 
            by = c("first_vacc_date" = "date")) 

RouttPop = as.numeric("25652")

maxdate <- casesbyweek %>% 
  arrange(desc(start)) %>% 
  select(start) %>% 
  slice(1) %>% 
  pull()

wn <- weeksall %>% 
  select(start) %>%
  arrange(start) %>% 
  distinct() %>% 
  mutate(w = 1:n())

fun <- function(df) {
  df$first:df$until
}

# people are counted as partially vaccinated until 
# they are up to date or until the final week 
## this takes forever to run :(
partial <- utd2 %>% 
  filter(!is.na(firstvac_week)) %>% 
  select(patient_id, firstvac_week, utd_week) %>% 
  mutate(through = maxdate) %>% 
  left_join(wn, by = c("firstvac_week" = "start")) %>% 
  rename(first = w) %>% 
  left_join(wn, by = c("utd_week" = "start")) %>% 
  rename(utd = w) %>% 
  left_join(wn, by = c("through" = "start")) %>% 
  rename(max = w) %>% 
  mutate(until = ifelse(is.na(utd), max, utd - 1)) %>% 
  select(patient_id, first, until) %>% 
  group_by(patient_id) %>% 
  nest() %>% 
  mutate(partialweeks = map(data, fun)) %>% 
  select(patient_id, partialweeks) %>% 
  unnest(cols = c(partialweeks)) %>% 
  ungroup() %>% 
  count(partialweeks)

populationbyweek <- tibble(week = wn$start) %>% 
  left_join(utd2 %>% 
              filter(!is.na(utd_week)) %>% 
              count(utd_week) %>% 
              rename(vaccinated = n) %>% 
              ungroup() %>% 
              arrange(utd_week) %>% 
              rename(week = utd_week) %>% 
              mutate(vaccinated = cumsum(vaccinated)) %>% 
              ungroup(), by = "week") %>% 
  left_join(partial %>% 
              ungroup() %>% 
              left_join(wn, by = c("partialweeks" = "w")) %>% 
              select(start, n) %>% 
              rename(week = start) %>% 
              arrange(week) %>% 
              rename(partial = n), by = "week") %>% 
  replace(.,is.na(.),0) %>%
  mutate(pop = RouttPop,
         unvaccinated = pop - partial - vaccinated) %>% 
  select(week, vaccinated, unvaccinated)

efficacy <- tibble(week = wn$start) %>% 
  left_join(casesbyweek %>% 
              rename(week = start)) %>% 
  left_join(populationbyweek %>% rename(vax_pop = vaccinated, 
                                        unvax_pop = unvaccinated)) %>% 
  mutate(unvax_ratio = unvaccinated / unvax_pop, 
         vax_ratio = breakthrough / vax_pop, 
         efficacy = (unvax_ratio - vax_ratio) / unvax_ratio) %>%
  replace(.,is.na(.),0) %>%
  filter(week<=floor_date(max(cases2$reporteddate), "week", week_start = getOption("lubridate.week.start", 1)))


points <- efficacy %>% 
  filter(week == max(week)) %>% 
  mutate(label = paste0(round(efficacy*100, 1), "%"))

dates <- wn %>% 
  mutate(month = month.abb[month(start)], 
         year = year(start)) %>% 
  group_by(month, year) %>% 
  filter(start == min(start)) %>% 
  ungroup()

# 
# #GRAPH - created in RMarkdown report
# ##requires plotly
# library(plotly)
# 
# 
# efficacy %>% 
#   mutate(efficacy = round(efficacy*100,2)) %>%
#   filter(week>="2021-02-01") %>%
#   left_join((ConfProbCases %>%
#                group_by(AttributionWeekStart) %>%
#                summarise(NumberCases = n()) %>%
#                right_join((Calendar %>%
#                              select(WeekStart) %>%
#                              distinct()), by = c("AttributionWeekStart" = "WeekStart")) %>%
#                replace(., is.na(.), 0) %>%
#                arrange(AttributionWeekStart) %>%
#                filter(AttributionWeekStart<=MostRecentWeekStart&AttributionWeekStart>"2020-02-21") %>%
#                mutate(TwoWeekCases=zoo::rollsumr(NumberCases, k = 2, fill = NA),
#                       TwoWeekRate=(TwoWeekCases/25652*100000)) %>%
#                mutate(TwoWeekStartDate = AttributionWeekStart-7,
#                       TwoWeekEndDate = AttributionWeekStart+6)), by = c("week" = "AttributionWeekStart")) %>%
#   plot_ly(x = ~week,
#           y = ~efficacy,
#           type = "scatter",
#           mode = "lines",
#           color = I("#4E2B1F"),
#           name = "Vaccine Efficacy") %>%
#   add_trace(x = ~week,
#             y = ~TwoWeekCases,
#             type = "scatter",
#             name = "Two Week (14 day) Case Count",
#             color = I("#BB4628"),
#             mode = "lines",
#             yaxis = "y2") %>%
#   layout(margin = list(l=50,r=50,h=100),
#          title = list(text = "Point in Time Vaccine Efficacy by Week as Compared to Case Counts"),
#          xaxis = list(title ="Week Start"),
#          yaxis = list(title = "Vaccine Efficacy",
#                       range = c(0,100)),
#          yaxis2 = list(title = "Weekly Sum Cases",
#                        side = "right",
#                        overlaying = "y",
#                        rangemode = "tozero",
#                        showgrid = F),
#          font = list(family = "Arial", size = 12),
#          legend = list(orientation = 'h', y=-.25))
# 
# 
