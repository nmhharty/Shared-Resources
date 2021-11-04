#Script to load Dr J data and School Case Report data for schools analysis
#First authored 9/3/2021 by Nicole Harty
#Last update: 10/18/2021


# Pull from JUSTINA SQL database ------------------------------------------
library(DBI)
library(odbc)
con <- dbConnect(odbc::odbc(), .connection_string = "Driver={SQL Server};Server=s-vm10-ph01;database=JUSTINA", timeout = 10)
#DrJ dataframe for joining to CEDRS
DrJ <- dbGetQuery(con, "SELECT DateOpened, EventID, FirstName, LastName, FinalDisposition, Age, DOB, SymptomOnsetDate, LastDateWorkplace, SchoolType, SchoolName, 
                                SchoolInfoNotes, TransmissionType, ExposureOutbreak, ExposureOutbreakSetting, SpecCollectionDateDrJ, ZIP 
                        FROM
                        (SELECT * , 	ROW_NUMBER() OVER (PARTITION BY EventID ORDER BY CASE WHEN TestType = 'PCR' THEN 1 ELSE 999 END) as TestRank
                          FROM
                        (SELECT CAST(pt.date_opened as Date) as DateOpened, CAST(pt.event_id as int) as EventID, CAST(first_name as varchar(256)) as FirstName, 
		                       CAST(last_name as varchar(256)) as LastName, CAST(final_disposition as varchar(256)) as FinalDisposition, CAST(age as int) as Age, 
		                       CAST(dob as Date) as DOB, CAST(symptom_onset_date as Date) as SymptomOnsetDate, 
		                       CAST(last_date_at_workplace as Date) as LastDateWorkplace,	CAST (school_name as varchar(4000)) as SchoolName, 
		                       CAST(school_info_notes as varchar(4000)) as SchoolInfoNotes, CAST(transmission_type as varchar(256)) as TransmissionType, 
		                       CAST(exposure_outbreak as varchar(256)) as ExposureOutbreak, CAST(exposure_outbreak_setting as varchar(256)) as ExposureOutbreakSetting, 
                    		  CAST(exposure_outbreak_known as varchar(256)) as ExposureOutbreakKnown, CAST(test_type as varchar(256)) as TestType, 
                    		  CAST(labs.specimen_collection_date as varchar(256)) as SpecCollectionDateDrJ, 
                    		  CAST(school_type as varchar(256)) as SchoolType, CAST(address_zip as varchar(256)) as ZIP
                    		FROM JUSTINA.dbo.patient pt
                    		    LEFT JOIN JUSTINA.dbo.lab_result labs
                    		      on pt.event_id = labs.event_id 
                    		    WHERE address_county = 'Routt'
                    		          and final_disposition <> 'duplicate' and final_disposition <> 'ooj' and final_disposition <> 'not_a_case') a
                    		      ) b
                    		WHERE TestRank = 1")
DrJ$EventID <- as.numeric((DrJ$EventID))

# Join Dr J and CEDRS -----------------------------------------------------

#Create df of all cases in Dr J that have a match in CEDRS
DRJcedrs <- DrJ %>%
  left_join(ConfProbCases %>%
              select(eventid, profileid, casestatus, gender, date_of_birth, city, zipcode, hospitalized, onsetdate, earliest_collectiondate, 
                     AttributionDate, reporteddate, AttributionWeekStart), by = c("EventID" = "eventid")) %>%
  filter(FinalDisposition %in% c("reached_completed", "reached_not_completed", "unreachable", ""))

#Add to DrJ data frame employer/school name from CEDRS for those where it matches (this is to include the CDE data)
## 10/18/2021 MUST USE NOVELCORONAVIRUS REPORT FOR THIS SO NEED TO REVISIT
# DrJ <- DrJ %>%
#   left_join(AllCases %>% 
#               select(EventID, EmployerSchoolName1, EmployerSchoolName2, AttributionDate, ReportedDate, AttributionWeekStart, ZipCode), by = "EventID")

# Add Columns to DRJcedrs and DrJ dataframes ------------------------------

##Add columns to joined df
colnames(DRJcedrs)
DRJcedrs <- DRJcedrs %>%
  mutate(Region = case_when(zipcode %in% c("80477", "80487", "80488") ~ "Steamboat Springs",
                            zipcode=="80428" ~ "North Routt",
                            zipcode=="80467" ~ "South Routt",
                            zipcode=="80469" ~ "South Routt",
                            zipcode=="80479" ~ "South Routt",
                            zipcode=="80483" ~ "South Routt",
                            zipcode=="81639" ~ "West Routt",
                            TRUE ~ as.character("Incomplete or Missing")),
         SchoolAge = case_when(Age>4&Age<18 ~ "Yes",
                               TRUE ~ as.character("No")),
         Under18 = case_when(Age<18 ~ "Yes",
                             TRUE ~ as.character("No")),
         TimePeriod = case_when(AttributionDate<"2020-10-20" ~ "Pre Dr J" ,
                                AttributionDate<"2021-06-01" ~ "School Year 2020-21",
                                AttributionDate<"2021-08-15" ~ "Summer 2021",
                                AttributionDate<"2022-06-01" ~ "School Year 2021-22 to date"))

#set factor levels
DRJcedrs$TimePeriod <- as.factor(DRJcedrs$TimePeriod)
DRJcedrs$TimePeriod <- fct_relevel(DRJcedrs$TimePeriod, c("Pre Dr J", "School Year 2020-21", "Summer 2021", "School Year 2021-22 to date"))

##Add columns to just Dr J df
colnames(DrJ)
DrJ[,c(1,7:9,16)] <- lapply(DrJ[,c(1,7:9,16)], as.Date.character)

DrJ <- DrJ %>%
  mutate(Region = case_when(ZIP %in% c("80477", "80487", "80488") ~ "Steamboat Springs",
                            ZIP=="80428" ~ "North Routt",
                            ZIP=="80467" ~ "South Routt",
                            ZIP=="80469" ~ "South Routt",
                            ZIP=="80479" ~ "South Routt",
                            ZIP=="80483" ~ "South Routt",
                            ZIP=="81639" ~ "West Routt",
                            TRUE ~ as.character("Incomplete or Missing")),
         SchoolAge = case_when(Age>4&Age<18 ~ "Yes",
                               TRUE ~ as.character("No")),
         Under18 = case_when(Age<18 ~ "Yes",
                             TRUE ~ as.character("No")),
         ReportedDateWeek = isoweek(DateOpened), 
         ReportedWeekStart = floor_date(DateOpened, "week", week_start = getOption("lubridate.week.start", 1)),
         CollectionDateWeek = isoweek(SpecCollectionDateDrJ), 
         CollectionWeekStart = floor_date(SpecCollectionDateDrJ, "week", week_start = getOption("lubridate.week.start", 1)),
         AttributionWeekStart = case_when(is.na(SpecCollectionDateDrJ) ~ ReportedWeekStart,
                                          TRUE ~ SpecCollectionDateDrJ),
         AttributionDate = case_when(is.na(SpecCollectionDateDrJ) ~ DateOpened,
                                     TRUE ~ SpecCollectionDateDrJ),
         TimePeriod = case_when(AttributionDate<"2020-10-20" ~ "Pre Dr J" ,
                                AttributionDate<"2021-06-01" ~ "School Year 2020-21",
                                AttributionDate<"2021-08-15" ~ "Summer 2021",
                                AttributionDate<"2022-06-01" ~ "School Year 2021-22 to date"))

#set factor levels
DrJ$TimePeriod <- as.factor(DrJ$TimePeriod)
DrJ$TimePeriod <- fct_relevel(DrJ$TimePeriod, c("Pre Dr J", "School Year 2020-21", "Summer 2021", "School Year 2021-22 to date"))


# School-Age Specific Data Frame for Dashboards and QA --------------------
#Dr J CEDRS joined df of school age
DRJcedrsSchoolAge <- DRJcedrs %>%
  filter(Age>4&Age<19)

#Just Dr J school age data (includes cases that haven't synced to CEDRS)
DrJSchoolAge <- DrJ %>%
  filter(Age>4&Age<19)


# Create Single School Name Column ------------------------------

#Look at cases just since Aug 10, 2021 (those associated with school year 2021-22)
DrJSchoolAge[,c(11:12,18:19)] <- lapply(DrJSchoolAge[,c(11:12,18:19)], str_to_lower)

#commenting out lines that use CEDRS Employer Name from NovelCoronavirusReport because this is using LPHA portal cedrs_Routt.txt

DrJSchoolAge <- DrJSchoolAge %>%
  mutate(SchoolNameClean = case_when( #SODA CREEK
                                     str_detect(SchoolName, 
                                                paste0(c("soda creek","sce"),collapse = '|')) ~ "SSSD Soda Creek Elementary",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("soda creek","sce"),collapse = '|')) ~ "SSSD Soda Creek Elementary",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("soda creek","sce"),collapse = '|')) ~ "SSSD Soda Creek Elementary",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("soda creek","sce"),collapse = '|')) ~ "SSSD Soda Creek Elementary",
                                     #SSMS
                                     str_detect(SchoolName, 
                                                paste0(c("ssms","SS middle","steamboat middle school", "steamboat springs middle school"),
                                                       collapse = '|')) ~ "SSSD Steamboat Springs Middle School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("ssms","SS middle","steamboat middle", "steamboat springs middle"),
                                                       collapse = '|')) ~ "SSSD Steamboat Springs Middle School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("ssms","SS middle","steamboat middle", "steamboat springs middle"),
                                     #                   collapse = '|')) ~ "SSSD Steamboat Springs Middle School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("ssms","SS middle","steamboat middle", "steamboat springs middle"),
                                     #                   collapse = '|')) ~ "SSSD Steamboat Springs Middle School",
                                     #Sleeping Giant
                                     str_detect(SchoolName, 
                                                paste0(c("sgs","sleeping giant"),collapse = '|')) ~ "SSSD Sleeping Giant School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("sgs","sleeping giant"),collapse = '|')) ~ "SSSD Sleeping Giant School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("sgs","sleeping giant"),collapse = '|')) ~ "SSSD Sleeping Giant School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("sgs","sleeping giant"),collapse = '|')) ~ "SSSD Sleeping Giant School",
                                     #SSHS
                                     str_detect(SchoolName, 
                                                paste0(c("sshs","steamboat springs high", "steamboat high"),
                                                       collapse = '|')) ~ "SSSD Steamboat Springs High School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("sshs","steamboat springs high", "steamboat high"),
                                                       collapse = '|')) ~ "SSSD Steamboat Springs High School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("sshs","steamboat springs high", "steamboat high"),
                                     #                   collapse = '|')) ~ "SSSD Steamboat Springs High School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("sshs","steamboat springs high", "steamboat high"),
                                     #                   collapse = '|')) ~ "SSSD Steamboat Springs High School",
                                     #Strawberry Park
                                     str_detect(SchoolName, 
                                                paste0(c("strawberry park","spe"),collapse = '|')) ~ "SSSD Strawberry Park Elementary",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("strawberry park","spe"),collapse = '|')) ~ "SSSD Strawberry Park Elementary",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("strawberry park","spe"),collapse = '|')) ~ "SSSD Strawberry Park Elementary",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("strawberry park","spe"),collapse = '|')) ~ "SSSD Strawberry Park Elementary",
                                     #YVHS
                                     str_detect(SchoolName, 
                                                paste0(c("yvhs","yampa valley"),collapse = '|')) ~ "SSSD Yampa Valley High School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("yvhs","yampa valley"),collapse = '|')) ~ "SSSD Yampa Valley High School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("yvhs","yampa valley"),collapse = '|')) ~ "SSSD Yampa Valley High School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("yvhs","yampa valley"),collapse = '|')) ~ "SSSD Yampa Valley High School",
                                     #SOROCO High
                                     str_detect(SchoolName, 
                                                paste0(c("soroco high","saroco high","south routt high","soroco hs"),collapse = '|')) ~ "SOROCO High School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("soroco high","saroco high","south routt high","soroco hs"),collapse = '|')) ~ "SOROCO High School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("soroco high","saroco high","south routt high","soroco hs"),collapse = '|')) ~ "SOROCO High School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("soroco high","saroco high","south routt high","soroco hs"),collapse = '|')) ~ "SOROCO High School",
                                     #SOROCO Middle
                                     str_detect(SchoolName, 
                                                paste0(c("soroco middle","saroco middle", "south routt middle"),collapse = '|')) ~ "SOROCO Middle School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("soroco middle","saroco middle", "south routt middle"),collapse = '|')) ~ "SOROCO Middle School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("soroco middle","saroco middle", "south routt middle"),collapse = '|')) ~ "SOROCO Middle School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("soroco middle","saroco middle", "south routt middle"),collapse = '|')) ~ "SOROCO Middle School",
                                     #SOROCO Elementary
                                     str_detect(SchoolName, 
                                                paste0(c("soroco elementary","saroco elementary","south routt elementary"),
                                                       collapse = '|')) ~ "South Routt Elementary School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("soroco elementary","saroco elementary","south routt elementary"),
                                                       collapse = '|')) ~ "South Routt Elementary School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("soroco elementary","saroco elementary","south routt elementary"),
                                     #                   collapse = '|')) ~ "South Routt Elementary School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("soroco elementary","saroco elementary","south routt elementary"),
                                     #                   collapse = '|')) ~ "South Routt Elementary School",
                                     #SOROCO Preschool at Yampa
                                     str_detect(SchoolName, 
                                                paste0(c("soroco pre","yampa pre", "south routt pre"),collapse = '|')) ~ "SOROCO Preschool at Yampa",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("soroco pre","yampa pre", "south routt pre"),collapse = '|')) ~ "SOROCO Preschool at Yampa",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("soroco pre","yampa pre", "south routt pre"),collapse = '|')) ~ "SOROCO Preschool at Yampa",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("soroco pre","yampa pre", "south routt pre"),collapse = '|')) ~ "SOROCO Preschool at Yampa",
                                     #Hayden Elementary
                                     str_detect(SchoolName, 
                                                paste0(c("hayden elementary","hayden schools elementary", "hayden valley elementary"),
                                                       collapse = '|')) ~ "Hayden Elementary",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("hayden elementary","hayden schools elementary", "hayden valley elementary"),
                                                       collapse = '|')) ~ "Hayden Elementary",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("hayden elementary","hayden schools elementary", "hayden valley elementary"),
                                     #                   collapse = '|')) ~ "Hayden Elementary",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("hayden elementary","hayden schools elementary", "hayden valley elementary"),
                                     #                   collapse = '|')) ~ "Hayden Elementary",
                                     #Hayden Secondary
                                     str_detect(SchoolName, 
                                                paste0(c("hayden middle","hayden schools middle", "hayden valley middle", "hayden hs", "hayden ms", 
                                                         "hayden valley high", "hayden secondary"),
                                                       collapse = '|')) ~ "Hayden Secondary",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("hayden middle","hayden schools middle", "hayden valley middle", "hayden hs", "hayden ms", 
                                                         "hayden valley high", "hayden secondary"),
                                                       collapse = '|')) ~ "Hayden Secondary",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("hayden middle","hayden schools middle", "hayden valley middle", "hayden hs", "hayden ms", 
                                     #                     "hayden valley high", "hayden secondary"),
                                     #                   collapse = '|')) ~ "Hayden Secondary",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("hayden middle","hayden schools middle", "hayden valley middle", "hayden hs", "hayden ms", 
                                     #                     "hayden valley high", "hayden secondary"),
                                     #                   collapse = '|')) ~ "Hayden Secondary",
                                     #Steamboat Montessori
                                     str_detect(SchoolName, 
                                                paste0(c("montessori","heritage park"),collapse = '|')) ~ "Steamboat Montessori",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("montessori","heritage park"),collapse = '|')) ~ "Steamboat Montessori",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("montessori","heritage park"),collapse = '|')) ~ "Steamboat Montessori",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("montessori","heritage park"),collapse = '|')) ~ "Steamboat Montessori",
                                     #Steamboat Mountain School
                                     str_detect(SchoolName, 
                                                paste0(c("mountain school", "mtn school"),collapse = '|')) ~ "Steamboat Mountain School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("mountain school", "mtn school"),collapse = '|')) ~ "Steamboat Mountain School",
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("mountain school", "mtn school"),collapse = '|')) ~ "Steamboat Mountain School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("mountain school", "mtn school"),collapse = '|')) ~ "Steamboat Mountain School",
                                     #NRCCS
                                     str_detect(SchoolName, 
                                                paste0(c("north routt charter", "nrccs", "north routt community school", "north routt community charter"),
                                                       collapse = '|')) ~ "North Routt Community Charter School",
                                     str_detect(SchoolInfoNotes, 
                                                paste0(c("north routt charter", "nrccs", "north routt community school", "north routt community charter"),
                                                       collapse = '|')) ~ "North Routt Community Charter School"
                                     #,
                                     # str_detect(EmployerSchoolName1, 
                                     #            paste0(c("north routt charter", "nrccs", "north routt community school", "north routt community charter"),
                                     #                   collapse = '|')) ~ "North Routt Community Charter School",
                                     # str_detect(EmployerSchoolName2, 
                                     #            paste0(c("north routt charter", "nrccs", "north routt community school", "north routt community charter"),
                                     #                   collapse = '|')) ~ "North Routt Community Charter School")
         ))
  

# Create Google Sheet for Data Review and QA ------------------------------

DrJSchoolAge <- DrJSchoolAge %>%
  mutate(LastUpdate = Sys.time())
# gs4_create(name = "SchoolAge Case Review", sheets = DrJSchoolAge) #this creates the sheet the first time
sheet_write(DrJSchoolAge, ss = "https://docs.google.com/spreadsheets/d/1G-bG7pL5jG7B6Qv6bEi2RG2imN2MVFMRErpenUF5Hkg/edit#gid=1114427213", 
            sheet = "DrJSchoolAge")  #this updates the existing sheet

# School COVID Case Report Form Data Wrangling ----------------------------
SchoolCaseReportForm <- range_read("https://docs.google.com/spreadsheets/d/1IS5XZL6i4xsnJEegykJYlik6BARRfetXbrQvpMYRd4g/edit?resourcekey#gid=164583539",
                                   sheet = 2)


# Region Summaries and Region Pop --------------------------------------------------------------

#Some addresses cannot be used -- how should I handle missing Region in my incidence rates and counts?? run county cumulative including and excluding region

RegionSummaryUnder18 <- DRJcedrs %>%
  group_by(Region, Under18, TimePeriod) %>%
  summarise(CaseCount = n()) %>%
  left_join(RouttPopGeog, by = c("Region" = "City"))

RegionSummarySchoolAge <- DRJcedrs %>%
  group_by(Region, SchoolAge, TimePeriod) %>%
  summarise(CaseCount = n()) %>%
  left_join(RouttPopGeog, by = c("Region" = "City"))

#School Age Population
SchoolAgePop <- RouttPopTable2 %>%
  filter(AGE>4,AGE<18) %>%
  summarise(SchoolAgePop = sum(TOTAL)) %>%
  pull(SchoolAgePop)

NONSchoolAgePop <- RouttPopTable2 %>%
  filter(AGE<5|AGE>17) %>%
  summarise(NONSchoolAgePop = sum(TOTAL)) %>%
  pull(NONSchoolAgePop)



# Notes and Enhancements -------------------------------------------------------------------


#cumulative incidence rate by region by TimePeriod

#census for each school to do rates? what's in CEDRS from Dept of Ed? can I use that? can I get the original source? 
##what takes precedence btwn CEDRS/Dept Ed feed and 

#positivity by region ??

#line graph of 2week incidence rate by region -- add vertical lines for school breaks, etc

#look at school outbreaks

#distribution of cases by age groups (elementary, middle, high)


#manually overriding dates in case report form responses spreadsheet doesn't lead to correct calculations