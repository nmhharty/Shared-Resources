#Script to load outbreak info from Google Drive and prep for analysis
#First authored 10/15/2021 by Nicole Harty
#Last update: 10/15/2021


# Outbreaks ---------------------------------------------------------------

Outbreaks <- range_read("https://docs.google.com/spreadsheets/d/1mLEWVyOADxp4FsVBvGjKpJFM97jpImAhsxQVUI1Ga1Q/edit#gid=167774008",
                        sheet = "Dashboard Source Sheet", range = "A:N")
Outbreaks <- Outbreaks %>%
  mutate(IsMostRecent4WeeksRolling = case_when(`Start Date` %within% RollingRecent4Week ~ "Yes",
                                               TRUE ~ as.character("No")),
         ActiveClosed = case_when(`End Date (Update as Needed)`<params$DataThroughDate ~ "Closed",
                                  TRUE ~ as.character("Active")))

SchoolOutbreaks <- Outbreaks %>%
  filter(Type2=="School/Childcare", Status!="Under Investigation") %>%
  mutate("Academic Year" = case_when(`Start Date` < "2021-06-01" ~ "2020-21",
                                     `Start Date` < "2021-08-01" ~ "Summer 2021",
                                     `Start Date` < "2022-06-01" ~ "2021-22 to date",
                                     `Start Date` < "2022-08-01" ~ "Summer 2022 to date",
                                     `Start Date` < "2023-06-01" ~ "2022-23 to date",
                                     TRUE ~ as.character("Not during academic year.")),
         "School or District" = case_when(str_detect(`Outbreak Name`,"SSHS")|str_detect(`Outbreak Name`,"SSMS")|
                                            str_detect(`Outbreak Name`,"Strawberry Park")|str_detect(`Outbreak Name`,"Soda Creek")|
                                            str_detect(`Outbreak Name`,"Sleeping Giant") ~ "Steamboat Springs School District",
                                          str_detect(`Outbreak Name`,"SOROCO")|str_detect(`Outbreak Name`,"Yampa")|
                                            str_detect(`Outbreak Name`,"South Routt") ~ "SOROCO School District",
                                          str_detect(`Outbreak Name`,"North Routt") ~ "North Routt Community Charter School",
                                          str_detect(`Outbreak Name`,"Steamboat Mountain School")|
                                            str_detect(`Outbreak Name`, "Emerald Mountain School") ~ "Steamboat Mountain School",
                                          str_detect(`Outbreak Name`,"Hayden") ~ "Hayden School District",
                                          TRUE ~ as.character("Other")))

SchoolOutbreaks$`Academic Year` <- factor(SchoolOutbreaks$`Academic Year`, levels = c("2020-21", "Summer 2021", "2021-22 to date", "Summer 2022 to date", 
                                                                                      "2022-23 to date", "Not during academic year."))

