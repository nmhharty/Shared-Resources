#Script to conduct ad-hoc analyses before determining need for on-going use and saving separately
#First authored 11/10/2020 by Nicole Harty
#Last update: 11/16/2021
#


# SOROCO positivity comparison --------------------------------------------



ROUTTpositivity <- ConfProbCases %>%
  filter(!is.na(collectiondate)) %>%
  group_by(AttributionDate) %>%
  summarise(Cases = n()) %>%
  full_join(DeDupNonAntibodyTests %>%
              group_by(collectiondate) %>%
              summarise(TotalTests = n()),
            by = c("AttributionDate" = "collectiondate")) %>%
  #  replace(., is.na(.), 0) %>%
  right_join((Calendar %>%
                select(date) %>%
                filter(date>"2020-02-29"&date<=(params$DataThroughDate))), by = c("AttributionDate" = "date")) %>%
  replace(., is.na(.), 0) %>%
  arrange(AttributionDate) %>%
  mutate(Sum14DayCases = zoo::rollsumr(Cases, k=14, fill=NA, na.rm=TRUE),
         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
         Rolling14DayPositivity = (Sum14DayCases/Sum14DayTests)*100)


  left_join(data.frame(City=c("North Routt", "South Routt", "West Routt", "Steamboat Springs"),
                       Pop=c(706,3194,2463,17212))

SOROCOdeDupeTest <- NonAntibodyTests %>%
  mutate(CITY = case_when(zipcode %in% c("80477", "80487", "80488") ~ "Steamboat Springs",
                          zipcode=="80428" ~ "North Routt",
                          zipcode=="80467" ~ "South Routt",
                          zipcode=="80469" ~ "South Routt",
                          zipcode=="80479" ~ "South Routt",
                          zipcode=="80483" ~ "South Routt",
                          zipcode=="81639" ~ "West Routt",
                          TRUE ~ as.character("Incomplete"))) %>%
  filter(CITY=="South Routt") %>%
  group_by(ID, collectiondate, CollectionDateWeek, CollectionWeekStart,
           # , IsMostRecent2Weeks,
           IsMostRecent2WeeksRolling, IsMostRecentWeekRolling, IsMostRecent2WeekComparison, IsMostRecent1WeekComparison) %>%
  summarise(DailyTestNum = row_number()) %>%
  filter(DailyTestNum==1)

                        
            
            
SOROCOpositivity <- ConfProbCases %>%
  filter(!is.na(collectiondate)) %>%
  group_by(AttributionDate) %>%
  summarise(Cases = n()) %>%
  full_join(SOROCOdeDupeTest %>%
              group_by(collectiondate) %>%
              summarise(TotalTests = n()),
            by = c("AttributionDate" = "collectiondate")) %>%
  #  replace(., is.na(.), 0) %>%
  right_join((Calendar %>%
                select(date) %>%
                filter(date>"2020-02-29"&date<=(params$DataThroughDate))), by = c("AttributionDate" = "date")) %>%
  replace(., is.na(.), 0) %>%
  arrange(AttributionDate) %>%
  mutate(Sum14DayCases = zoo::rollsumr(Cases, k=14, fill=NA, na.rm=TRUE),
         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
         Rolling14DayPositivitySOROCO = (Sum14DayCases/Sum14DayTests)*100)

SOROCOpositivity2 <- NonAntibodyTests %>%
  mutate(CITY = case_when(zipcode %in% c("80477", "80487", "80488") ~ "Steamboat Springs",
                          zipcode=="80428" ~ "North Routt",
                          zipcode=="80467" ~ "South Routt",
                          zipcode=="80469" ~ "South Routt",
                          zipcode=="80479" ~ "South Routt",
                          zipcode=="80483" ~ "South Routt",
                          zipcode=="81639" ~ "West Routt",
                          TRUE ~ as.character("Incomplete"))) %>%
  filter(CITY=="South Routt", covid19_test_result=="POSITIVE") %>%
  group_by(collectiondate) %>%
  summarise(POStests = n()) %>%
  full_join(SOROCOdeDupeTest %>%
              group_by(collectiondate) %>%
              summarise(TotalTests = n()),
            by = "collectiondate") %>%
  #  replace(., is.na(.), 0) %>%
  right_join((Calendar %>%
                select(date) %>%
                filter(date>"2020-02-29"&date<=(params$DataThroughDate))), by = c("collectiondate" = "date")) %>%
  replace(., is.na(.), 0) %>%
  arrange(collectiondate) %>%
  mutate(Sum14DayPOStests = zoo::rollsumr(POStests, k=14, fill=NA, na.rm=TRUE),
         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
         Rolling14DayPositivitySOROCO = (Sum14DayPOStests/Sum14DayTests)*100)


ROUTTpositivity %>%
  select(AttributionDate, Rolling14DayPositivity) %>%
  left_join(SOROCOpositivity2 %>% 
              select(collectiondate, Rolling14DayPositivitySOROCO), by = c("AttributionDate" = "collectiondate")) %>%
  filter(AttributionDate>="2020-10-05") %>%
  plot_ly(x = ~AttributionDate,
          y = ~Rolling14DayPositivity,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y",
          name = "Routt Positivity") %>%
  add_trace(x = ~AttributionDate,
            y = ~Rolling14DayPositivitySOROCO,
            type = "scatter",
            name = "SOROCO Positiviity",
            mode = "lines",
            color = I("#BB4628")) %>%
  # add_trace(data = POStxtTable %>%
  #             filter(AttributionDate>="2020-10-05"),
  #           x = ~AttributionDate,
  #           y = ~Rolling14DayPositivity,
  #           type = "scatter",
  #           name = "Overall Positiviity",
  #           mode = "lines",
  #           color = I("#F2BF4C")) %>%
  layout(margin = list(l=50,r=50,b=155,h=175),
         title = "Rolling 14-day Positivity by Test Type Compared to County Total",
         xaxis = list(type = 'date',
                      tickformat = "%b %d, %Y",
                      autotick = F, 
                      tickangle = 45,
                      tick0 = "2000-01-10",
                      dtick = 1209600000,
                      title = "Week Start (all dates are Mondays)",
                      showgrid = FALSE),
         yaxis = list(title = "Positivity Rate (%)"),
         legend = list(orientation = 'h', y=-.3),
         font = list(family = "Arial", size = 12)
         )


ROUTTpositivity %>%
  select(AttributionDate, Rolling14DayPositivity) %>%
  left_join(SOROCOpositivity2 %>% 
              select(collectiondate, Rolling14DayPositivitySOROCO), by = c("AttributionDate" = "collectiondate")) %>%
  filter(AttributionDate>="2020-10-05") %>%
  mutate(DIFFprop = Rolling14DayPositivitySOROCO/Rolling14DayPositivity) %>%
  plot_ly(x = ~AttributionDate,
          y = ~DIFFprop,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y",
          name = "Routt Positivity") %>%
  layout(margin = list(l=50,r=50,b=155,h=175),
         title = "Rolling 14-day Positivity by Test Type Compared to County Total",
         xaxis = list(type = 'date',
                      tickformat = "%b %d, %Y",
                      autotick = F, 
                      tickangle = 45,
                      tick0 = "2000-01-10",
                      dtick = 1209600000,
                      title = "Week Start (all dates are Mondays)",
                      showgrid = FALSE),
         yaxis = list(title = "Positivity Rate (%)"),
         legend = list(orientation = 'h', y=-.3),
         font = list(family = "Arial", size = 12)
  )


# Hospitalizations --------------------------------------------------------


COPHS %>%
  filter(Hospital.Admission.Date...MM.DD.YYYY.>"2021-08-01") %>%
  group_by(City) %>%
  count()


# Vaccine Breakthrough and Efficacy ---------------------------------------

VaxBreakthroughList %>%
  count()

ConfProbCases %>%
  filter(breakthrough==TRUE) %>%
  count()

# CDPHE PORTAL FILE identifies 564 breakthrough as of 11/16. Routt identified through code = 506
# updating VaxData.R script to use CDPHE breakthrough column



levels(ConfProbCases$vaccine_received)

ConfProbCases %>%
  select(eventid, reporteddate,vax_utd14,vaccine_received, vax_firstdose, partialonly, vax_utd, breakthrough, FullyVaxAtCaseReport) %>%
  filter(breakthrough==TRUE) %>%
  mutate(CHECK = vax_utd14-reporteddate) %>%
  arrange(desc(CHECK))


