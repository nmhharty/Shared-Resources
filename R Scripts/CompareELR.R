#Script to compare CEDRS ELR csv to LPHA portal txt files
#First authored 10/19/2021 by Nicole Harty
#Last update: 10/19/2021
#


# POSITIVITY txt ----------------------------------------------------------

POStxtTable <- ConfProbCases %>%
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

POStxtTable2 <- ConfProbCases %>%
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

plot_ly(data = (ConfProbCases %>%
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
                # DailyPositivity=(Cases/TotalTests)*100,
                #  Rolling14DayPositivity=zoo::rollmeanr(DailyPositivity, k = 14, fill = NA, na.rm=TRUE))
),
x = ~AttributionDate,
y = ~Rolling14DayPositivity,
type = "scatter",
name = "Rolling 14-day Positivity Rate",
yaxis = "y",
mode = "lines",
color = I("#4E2B1F")) %>%
  layout(margin = list(l=50,r=50,b=155,h=175),
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
         font = list(family = "Arial", size = 12),
         shapes = list(
           list(type = "rect",
                fillcolor = 'rgb(71, 118, 145)', line = list(color = 'rgb(71, 118, 145)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 0, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(187, 70, 40)', line = list(color = 'rgb(187, 70, 40)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 5, y1 = 10, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(78, 43, 31)', line = list(color = 'rgb(78, 43, 31)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 10, y1 = 20, yref = "y")
         ),
         annotations = list(
           list(x = .2,
                y = 2.5,
                text = "Low Risk",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#477691")),
           list(x = .2,
                y = 7.5,
                text = "Caution",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#BB4628")),
           list(x = .2,
                y = 15,
                text = "High Risk",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#4E2B1F")),
           list(x = .0,
                y = .5,
                text = "Data prepared by Routt County Public Health",
                xref = "paper",
                yref = 'paper',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "gray"))
         )
  )



# POSITIVITY csv ----------------------------------------------------------
POScsvTable <- ConfProbCasesXLSX %>%
  group_by(AttributionDate) %>%
  summarise(Cases = n()) %>%
  full_join(DeDupNonAntibodyTestsCSV %>%
              group_by(CollectionDate) %>%
              summarise(TotalTests = n()),
            by = c("AttributionDate" = "CollectionDate")) %>%
  #  replace(., is.na(.), 0) %>%
  right_join((Calendar %>%
                select(date) %>%
                filter(date>"2020-02-29"&date<=(DataThroughDate))), by = c("AttributionDate" = "date")) %>%
  replace(., is.na(.), 0) %>%
  arrange(AttributionDate) %>%
  mutate(Sum14DayCases = zoo::rollsumr(Cases, k=14, fill=NA, na.rm=TRUE),
         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
         Rolling14DayPositivity = (Sum14DayCases/Sum14DayTests)*100)



plot_ly(data = (ConfProbCasesXLSX %>%
                  group_by(AttributionDate) %>%
                  summarise(Cases = n()) %>%
                  full_join(DeDupNonAntibodyTestsCSV %>%
                              group_by(CollectionDate) %>%
                              summarise(TotalTests = n()),
                            by = c("AttributionDate" = "CollectionDate")) %>%
                  #  replace(., is.na(.), 0) %>%
                  right_join((Calendar %>%
                                select(date) %>%
                                filter(date>"2020-02-29"&date<=(DataThroughDate))), by = c("AttributionDate" = "date")) %>%
                  replace(., is.na(.), 0) %>%
                  arrange(AttributionDate) %>%
                  mutate(Sum14DayCases = zoo::rollsumr(Cases, k=14, fill=NA, na.rm=TRUE),
                         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
                         Rolling14DayPositivity = (Sum14DayCases/Sum14DayTests)*100)
                # DailyPositivity=(Cases/TotalTests)*100,
                #  Rolling14DayPositivity=zoo::rollmeanr(DailyPositivity, k = 14, fill = NA, na.rm=TRUE))
),
x = ~AttributionDate,
y = ~Rolling14DayPositivity,
type = "scatter",
name = "Rolling 14-day Positivity Rate",
yaxis = "y",
mode = "lines",
color = I("#4E2B1F")) %>%
  layout(margin = list(l=50,r=50,b=155,h=175),
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
         font = list(family = "Arial", size = 12),
         shapes = list(
           list(type = "rect",
                fillcolor = 'rgb(71, 118, 145)', line = list(color = 'rgb(71, 118, 145)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 0, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(187, 70, 40)', line = list(color = 'rgb(187, 70, 40)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 5, y1 = 10, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(78, 43, 31)', line = list(color = 'rgb(78, 43, 31)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 10, y1 = 20, yref = "y")
         ),
         annotations = list(
           list(x = .2,
                y = 2.5,
                text = "Low Risk",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#477691")),
           list(x = .2,
                y = 7.5,
                text = "Caution",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#BB4628")),
           list(x = .2,
                y = 15,
                text = "High Risk",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#4E2B1F")),
           list(x = .0,
                y = .5,
                text = "Data prepared by Routt County Public Health",
                xref = "paper",
                yref = 'paper',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "gray"))
         )
  )



# POSITIVITY txt NUM=TESTS ----------------------------------------------------------

POStxtTable <- ConfProbCases %>%
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

DeDupNonAntibodyTests %>%
  filter(Lab=="Routt County Providers") %>%
  filter(!is.na(collectiondate)) %>%
  mutate(ExcludeStateLab = case_when(TestingLab=="CDPHE - State Lab"&collectiondate>"2020-11-22"&collectiondate<"2021-03-01" ~ "Yes",
                                     TestingLab=="Curative, Inc."&collectiondate<"2020-11-16" ~ "Yes",
                                     TRUE ~ as.character("No"))) %>%
  filter(ExcludeStateLab=="No") %>%
  mutate(ReportDelay = resultdate-collectiondate) %>% 
  group_by(CollectionWeekStart, Lab) %>%
  summarise(AvgDelay = as.numeric(mean(ReportDelay, na.rm=TRUE))) %>%
  pivot_wider(names_from = Lab, values_from = AvgDelay) %>%
  rename("Routt County Providers Delay" = "Routt County Providers") %>%
  left_join(PCRtests %>%
              filter(Lab=="Routt County Providers") %>%
              filter(!is.na(collectiondate)) %>%
              mutate(Exclude = case_when(TestingLab=="CDPHE"&collectiondate>"2020-11-22"&collectiondate<"2021-03-01" ~ "Yes",
                                         TestingLab=="CURATIVE"&collectiondate<"2020-11-16" ~ "Yes",
                                         TRUE ~ as.character("No"))) %>%
              filter(Exclude=="No") %>%
              mutate(ReportDelay = receivedate-collectiondate) %>%  
              group_by(CollectionWeekStart, Lab) %>%
              summarise(NumTests = n()) %>%
              pivot_wider(names_from = Lab, values_from = NumTests) %>%
              rename("Routt County Providers Number of Tests" = "Routt County Providers"), by = "CollectionWeekStart") 


# POSITIVITY csv NUM=TESTS ----------------------------------------------------------
POScsvTable <- ConfProbCasesXLSX %>%
  group_by(AttributionDate) %>%
  summarise(Cases = n()) %>%
  full_join(DeDupNonAntibodyTestsCSV %>%
              group_by(CollectionDate) %>%
              summarise(TotalTests = n()),
            by = c("AttributionDate" = "CollectionDate")) %>%
  #  replace(., is.na(.), 0) %>%
  right_join((Calendar %>%
                select(date) %>%
                filter(date>"2020-02-29"&date<=(DataThroughDate))), by = c("AttributionDate" = "date")) %>%
  replace(., is.na(.), 0) %>%
  arrange(AttributionDate) %>%
  mutate(Sum14DayCases = zoo::rollsumr(Cases, k=14, fill=NA, na.rm=TRUE),
         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
         Rolling14DayPositivity = (Sum14DayCases/Sum14DayTests)*100)



plot_ly(data = (ConfProbCasesXLSX %>%
                  group_by(AttributionDate) %>%
                  summarise(Cases = n()) %>%
                  full_join(DeDupNonAntibodyTestsCSV %>%
                              group_by(CollectionDate) %>%
                              summarise(TotalTests = n()),
                            by = c("AttributionDate" = "CollectionDate")) %>%
                  #  replace(., is.na(.), 0) %>%
                  right_join((Calendar %>%
                                select(date) %>%
                                filter(date>"2020-02-29"&date<=(DataThroughDate))), by = c("AttributionDate" = "date")) %>%
                  replace(., is.na(.), 0) %>%
                  arrange(AttributionDate) %>%
                  mutate(Sum14DayCases = zoo::rollsumr(Cases, k=14, fill=NA, na.rm=TRUE),
                         Sum14DayTests = zoo::rollsumr(TotalTests, k=14, fill = NA, na.rm=TRUE),
                         Rolling14DayPositivity = (Sum14DayCases/Sum14DayTests)*100)
                # DailyPositivity=(Cases/TotalTests)*100,
                #  Rolling14DayPositivity=zoo::rollmeanr(DailyPositivity, k = 14, fill = NA, na.rm=TRUE))
),
x = ~AttributionDate,
y = ~Rolling14DayPositivity,
type = "scatter",
name = "Rolling 14-day Positivity Rate",
yaxis = "y",
mode = "lines",
color = I("#4E2B1F")) %>%
  layout(margin = list(l=50,r=50,b=155,h=175),
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
         font = list(family = "Arial", size = 12),
         shapes = list(
           list(type = "rect",
                fillcolor = 'rgb(71, 118, 145)', line = list(color = 'rgb(71, 118, 145)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 0, y1 = 5, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(187, 70, 40)', line = list(color = 'rgb(187, 70, 40)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 5, y1 = 10, yref = "y"),
           list(type = "rect",
                fillcolor = 'rgb(78, 43, 31)', line = list(color = 'rgb(78, 43, 31)'), opacity = 0.4,
                x0 = 0, x1 = 1, xref = "paper",
                y0 = 10, y1 = 20, yref = "y")
         ),
         annotations = list(
           list(x = .2,
                y = 2.5,
                text = "Low Risk",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#477691")),
           list(x = .2,
                y = 7.5,
                text = "Caution",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#BB4628")),
           list(x = .2,
                y = 15,
                text = "High Risk",
                xref = "paper",
                yref = 'y',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "#4E2B1F")),
           list(x = .0,
                y = .5,
                text = "Data prepared by Routt County Public Health",
                xref = "paper",
                yref = 'paper',
                showarrow = FALSE,
                font = list(family = "Arial", size = 12, color = "gray"))
         )
  )




# DELAY txt ----------------------------------------------------------
# PCRtests %>%
#   filter(collectiondate>"2021-08-01") %>%
#   group_by(TestingLab) %>%
#   count() %>%
# #  filter(n>200) %>%
#   arrange(desc(n))

PCRtests %>%
  filter(Lab=="Routt County Providers") %>%
  filter(!is.na(collectiondate)) %>%
  mutate(ExcludeStateLab = case_when(TestingLab=="CDPHE - State Lab"&collectiondate>"2020-11-22"&collectiondate<"2021-03-01" ~ "Yes",
                                     TestingLab=="Curative, Inc."&collectiondate<"2020-11-16" ~ "Yes",
                                     TRUE ~ as.character("No"))) %>%
  filter(ExcludeStateLab=="No") %>%
  mutate(ReportDelay = resultdate-collectiondate) %>% 
  group_by(CollectionWeekStart, Lab) %>%
  summarise(AvgDelay = as.numeric(mean(ReportDelay, na.rm=TRUE))) %>%
  pivot_wider(names_from = Lab, values_from = AvgDelay) %>%
  rename("Routt County Providers Delay" = "Routt County Providers") %>%
  left_join(PCRtests %>%
              filter(Lab=="Routt County Providers") %>%
              filter(!is.na(collectiondate)) %>%
              mutate(Exclude = case_when(TestingLab=="CDPHE"&collectiondate>"2020-11-22"&collectiondate<"2021-03-01" ~ "Yes",
                                         TestingLab=="CURATIVE"&collectiondate<"2020-11-16" ~ "Yes",
                                         TRUE ~ as.character("No"))) %>%
              filter(Exclude=="No") %>%
              mutate(ReportDelay = receivedate-collectiondate) %>%  
              group_by(CollectionWeekStart, Lab) %>%
              summarise(NumTests = n()) %>%
              pivot_wider(names_from = Lab, values_from = NumTests) %>%
              rename("Routt County Providers Number of Tests" = "Routt County Providers"), by = "CollectionWeekStart") %>%
  filter(CollectionWeekStart>(as.Date(params$MostRecentWeekStart)-42)) %>%
  filter(CollectionWeekStart<=params$MostRecentWeekStart) %>%
  mutate(textCountyProviders=paste0("Number of Tests: ",`Routt County Providers Number of Tests`,
                                    "<br>Avg Delay: ",round(`Routt County Providers Delay`,2)," days")) %>%
  plot_ly(x = ~CollectionWeekStart, y = ~AvgDelay) %>%
  add_trace(y = ~`Routt County Providers Delay`,
            name = "Routt County Providers",
            type = "bar",
            marker = list(color = "#BB4628"),
            text = ~textCountyProviders,
            hoverinfo = "text") %>%
  layout(xaxis = list(type = 'date',
                      tickformat = "%b %d, %Y",
                      autotick = F, 
                      tick0 = "2000-01-10",
                      dtick = 604800000,
                      title = "Week Start (all dates are Mondays)"),
         yaxis = list(title = "Average Delay (days)",
                      nticks = 6),
         font = list(family = "Arial", size = 12))




# DELAY csv ----------------------------------------------------------
# PCRtests %>%
#   filter(CollectionDate>"2021-03-01") %>%
#   group_by(PerformingLab#, Submitter
#            ) %>%
#   count() %>%
#   arrange(desc(n))
PCRtests %>%
  filter(PerformingLab %in% c("CDPHE - State Lab", "Other", "University Hospital", "Curative, Inc.", "Yampa Valley Medical Center",
                              "ARUP", "LabCorp", "Immunogeno", "ImmunoGenomics", "Fulgent Genetics (CA)", "National Jewish Health",
                              "Quest Diagnostics", "Aegis Sciences Corp (TN)", "ATCG Lab", "VRL Eurofins", "Mako Medical Lab", 
                              "VRLEUROFINSCO")) %>%
  filter(!is.na(CollectionDate)) %>%
  mutate(ExcludeStateLab = case_when(PerformingLab=="CDPHE - State Lab"&CollectionDate>"2020-11-22"&CollectionDate<"2021-03-01" ~ "Yes",
                                     PerformingLab=="Curative, Inc."&CollectionDate<"2020-11-16" ~ "Yes",
                                     TRUE ~ as.character("No"))) %>%
  filter(ExcludeStateLab=="No") %>%
  mutate(ReportDelay = ReportDate-CollectionDate,
         Lab = fct_collapse(PerformingLab, "Routt County Providers"=c("Other", "University Hospital", "Yampa Valley Medical Center",
                                                                      "ARUP", "LabCorp", "Immunogeno", "ImmunoGenomics", "Fulgent Genetics (CA)", "Quest Diagnostics", 
                                                                      "Aegis Sciences Corp (TN)", "CDPHE - State Lab", "National Jewish Health", "Curative, Inc.", "ATCG Lab",
                                                                      "VRL Eurofins", "Mako Medical Lab", "VRLEUROFINSCO"))) %>% 
  group_by(CollectionWeekStart, Lab) %>%
  summarise(AvgDelay = as.numeric(mean(ReportDelay, na.rm=TRUE))) %>%
  pivot_wider(names_from = Lab, values_from = AvgDelay) %>%
  rename("Routt County Providers Delay" = "Routt County Providers") %>%
  left_join(PCRtests %>%
              filter(PerformingLab %in% c("Other", "University Hospital", "Yampa Valley Medical Center", "ARUP", "LabCorp", "Immunogeno",
                                          "ImmunoGenomics", "Fulgent Genetics (CA)", "Quest Diagnostics", "Aegis Sciences Corp (TN)", 
                                          "CDPHE - State Lab", "National Jewish Health", "Curative, Inc.", "ATCG Lab", "VRL Eurofins", 
                                          "Mako Medical Lab", "VRLEUROFINSCO")) %>%
              filter(!is.na(CollectionDate)) %>%
              mutate(Exclude = case_when(PerformingLab=="CDPHE - State Lab"&CollectionDate>"2020-11-22"&CollectionDate<"2021-03-01" ~ "Yes",
                                         PerformingLab=="Curative, Inc."&CollectionDate<"2020-11-16" ~ "Yes",
                                         TRUE ~ as.character("No"))) %>%
              filter(Exclude=="No") %>%
              mutate(ReportDelay = ReportDate-CollectionDate, 
                     Lab = fct_collapse(PerformingLab, "Routt County Providers"=c("Other", "University Hospital", "Yampa Valley Medical Center",
                                                                                  "ARUP", "LabCorp", "Immunogeno", "ImmunoGenomics", 
                                                                                  "Fulgent Genetics (CA)", "Quest Diagnostics", 
                                                                                  "Aegis Sciences Corp (TN)", "CDPHE - State Lab", 
                                                                                  "National Jewish Health", "Curative, Inc.",
                                                                                  "ATCG Lab", "VRL Eurofins", "Mako Medical Lab",
                                                                                  "VRLEUROFINSCO"))) %>%  
              group_by(CollectionWeekStart, Lab) %>%
              summarise(NumTests = n()) %>%
              pivot_wider(names_from = Lab, values_from = NumTests) %>%
              rename("Routt County Providers Number of Tests" = "Routt County Providers"), by = "CollectionWeekStart") %>%
  filter(CollectionWeekStart>(MostRecentWeekStart-42)) %>%
  filter(CollectionWeekStart<=MostRecentWeekStart) %>%
  mutate(textCountyProviders=paste0("Number of Tests: ",`Routt County Providers Number of Tests`,
                                    "<br>Avg Delay: ",round(`Routt County Providers Delay`,2)," days")) %>%
  plot_ly(x = ~CollectionWeekStart, y = ~AvgDelay) %>%
  add_trace(y = ~`Routt County Providers Delay`,
            name = "Routt County Providers",
            type = "bar",
            marker = list(color = "#BB4628"),
            text = ~textCountyProviders,
            hoverinfo = "text") %>%
  layout(xaxis = list(type = 'date',
                      tickformat = "%b %d, %Y",
                      autotick = F, 
                      tick0 = "2000-01-10",
                      dtick = 604800000,
                      title = "Week Start (all dates are Mondays)"),
         yaxis = list(title = "Average Delay (days)",
                      nticks = 6),
         font = list(family = "Arial", size = 12))



# OTHER -------------------------------------------------------------------

PCRtestsCSV$MappedResult <- as.factor(PCRtestsCSV$MappedResult)

PCRtestsCSV %>%
  filter(PerformingLab=="Mako Medical Lab", MappedResult=="POSITIVE") %>%
  filter(!is.na(CollectionDate)) %>%
  # mutate(ExcludeStateLab = case_when(PerformingLab=="CDPHE - State Lab"&CollectionDate>"2020-11-22"&CollectionDate<"2021-03-01" ~ "Yes",
  #                                    PerformingLab=="Curative, Inc."&CollectionDate<"2020-11-16" ~ "Yes",
  #                                    TRUE ~ as.character("No"))) %>%
  # filter(ExcludeStateLab=="No") %>%
  group_by(CollectionWeekStart, PerformingLab) %>%
  summarise(PositiveTotal=n()) %>%
  left_join(PCRtestsCSV %>%
              filter(PerformingLab=="Mako Medical Lab") %>%
              filter(!is.na(CollectionDate)) %>%
              # mutate(Exclude = case_when(PerformingLab=="CDPHE - State Lab"&CollectionDate>"2020-11-22"&CollectionDate<"2021-03-01" ~ "Yes",
              #                            PerformingLab=="Curative, Inc."&CollectionDate<"2020-11-16" ~ "Yes",
              #                            TRUE ~ as.character("No"))) %>%
              # filter(Exclude=="No") %>%
              group_by(CollectionWeekStart, PerformingLab) %>%
              summarise(NumTests = n()), by = c("CollectionWeekStart", "PerformingLab")) %>%
  filter(CollectionWeekStart>(MostRecentWeekStart-42)) %>%
  filter(CollectionWeekStart<=MostRecentWeekStart) %>%
  mutate(Positivity=scales::percent(PositiveTotal/NumTests))


#trying to get deduplicated one row per person per date, and ID which is a positive result if discordant
NonAntibodyTests %>%
  group_by(person_id, collectiondate, covid19_test_result) %>% 
  summarise(DailyTestNum = row_number()) %>%
  filter(DailyTestNum==1)

#person_id only exists for PCR tests in the txt file

NonAntibodyTests %>%
  filter(person_id=="ALEYNAPORRECA1994-05-31") %>%
  select(person_id, collectiondate, test_type, submitter)
group_by(person_id, collectiondate, test_type) %>% 
  summarise(DailyTestNum = row_number()) %>%
  #  filter(test_type=="antigen") %>%
  arrange(desc(DailyTestNum))

NonAntibodyTestsCSV %>%
  filter(ID=="ALEYNAPORRECA1994-05-31") %>%
  select(ID, CollectionDate, TestType, PerformingLab)



# Compare Positivity Numerators -------------------------------------------

POStxtTableCompare <- ConfProbCases %>%
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
         Rolling14DayPositivity = (Sum14DayCases/Sum14DayTests)*100) %>%
  left_join(POStxtTable, by = "AttributionDate")

POStxtTableCompare <- POStxtTableCompare %>%
  mutate(POSdifference = Rolling14DayPositivity.x-Rolling14DayPositivity.y)

POStxtTableCompare %>%
  plot_ly(x = ~AttributionDate,
          y = ~POSdifference,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y") %>%
  layout(title = "Difference in Positivity Including Cases without Test MINUS Excluding <br> Negative means existing calculation is TOO HIGH")


POScsvTable %>%
  left_join(POStxtTable, by = "AttributionDate") %>%
  mutate(TestVolumeDif = Sum14DayTests.x-Sum14DayTests.y,
         PositivityDif = Rolling14DayPositivity.x-Rolling14DayPositivity.y) %>%
  plot_ly(x = ~AttributionDate,
          y = ~PositivityDif,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y") %>%
  layout(title = "Difference in Positivity CEDRS vs Portal <br> Negative means CEDRS is Greater than Portal")


POScsvTable %>%
  left_join(POStxtTable2, by = "AttributionDate") %>%
  mutate(TestVolumeDif = Sum14DayTests.x-Sum14DayTests.y,
         PositivityDif = Rolling14DayPositivity.x-Rolling14DayPositivity.y) %>%
  plot_ly(x = ~AttributionDate,
          y = ~PositivityDif,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y") %>%
  layout(title = "Difference in Positivity CEDRS ONLY POS TEST vs Portal <br> Positive means Portal is Greater than CEDRS")

POScsvTable %>%
  left_join(POStxtTable, by = "AttributionDate") %>%
  mutate(TestVolumeDif = Sum14DayTests.x-Sum14DayTests.y,
         PositivityDif = Rolling14DayPositivity.x-Rolling14DayPositivity.y) %>%
  plot_ly(x = ~AttributionDate,
          y = ~TestVolumeDif,
          type = "scatter",
          color = I("#4E2B1F"),
          mode = "lines",
          yaxis = "y") %>%
  layout(title = "Difference in Test Volume CEDRS vs Portal <br> Negative means CEDRS is Greater than Portal")