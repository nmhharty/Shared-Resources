# Shared-Resources
R scripts, child Rmd, images used by variety of COVID projects  
This repositroy contains directories for child Rmd files, R scripts, and images necessary to create both public and internal COVID dashboards for Routt County Public Health. Regular pushes include updates to data cleaning scripts or child RmD files.

# Folders and Files   
## Child Rmd Files  
This folder contains all the child Rmd files used by the public and/or internal COVID dashboards.  
- AllCasesSummary.Rmd: graph and associated narrative for high-level case summary
- BOH Presentation.Rmd: Full Rmd file to generate key graphs for board of health presentations (this is actually a parent markdown file that references other child Rmd files)  
- CaseSummaryToDate.Rmd: brief summary of case counts to date  
- CICT Trends.Rmd: graph and associated narrative for case investigation and contact tracing team metrics  
- Hayden Vax Detail.Rmd: generates table summarizing Hayden School District vaccination info  
- Incidence-7dayRollingAvg.Rmd: graph and associated narrative for 7 day incidence
- Incidence-Daily14dayRollingAvg.Rmd: graph and associated narrative for 14 day incidence  
- Incidence-SchoolAgeNonSchoolAge14dayRolling.Rmd: graph and associated narrative for comparing school age incidence to non-school age population incidence  
- Incidence-Weekly14dayRollingAvg.Rmd: graph and associated narrative for weekly bar chart of 14-day incidence  
- IncidencePositivyAgeSchoolCompare.Rmd: graph and associated narrative for comparing positivity by age group
- IncidenceReportDate.Rmd: graph and associated narrative for incidence using report date as CDPHE calculates incidence  
- INTERNAL RouttRoadToRecovery Dashboard.Rmd: old Rmd file of all internal dashboard code, not used, merely reference  
- Montessori Vax Detail.Rmd: generates table summarizing Montessori school vaccination info  
- MostRecent2Weeks1WeekSummary_Table.Rmd: generates table summarizing case info for last two weeks  
- OutbreakDataSlides.Rmd: generates slide set of outbreak data; one-off analysis  
- Outbreaks.Rmd: graph and associated narrative for outbreak data  
- PCRdelay.Rmd: graph and associated narrative for turn-around-time for PCR tests  
- Positivity.Rmd: graph and associated narrative for positivity both using PCR and antigen tests together and separately  
- ResidentVisitorWeeklyCases.Rmd: graph and associated narrative for comparing case counts of residents and visitors  
- RouttRoadToRecovery Dashboard.Rmd: old Rmd file that generated public dashboard in its entirety, not used, merely reference  
- RouttRoadToRecovery_Gauge.Rmd: generates gauge graphic of key metrics  
- Schools.Rmd: draft, not in use, for schools data analysis   
- SSSD Vax Detail.Rmd: generates table summarizing Steamboat Springs School District vaccination info  
- Surveillance-CaseOutcomes.Rmd: graph and associated narrative for summarizing case outcomes  
- Surveillance-ExposureTypeResidents.Rmd: graph and associated narrative for summarizing source of exposure for resident cases  
- Surveillance-ExposureTypeVisitors.Rmd: graph and associated narrative for summarizing source of exposure for visitor cases  
- Surveillance-Symptoms.Rmd: graph and associated narrative for symptom info of cases reached  
- Surveillance-VaxEfficacy.Rmd: graph and associated narrative for vaccine efficacy, vaccine breakthrough, and booster dose info  
- template.css: CSS file needs to live in this directory for testing child Rmd files  
- TotalCasesToDate_Table.Rmd: table with case counts for each day  
- VaccinationRatesAgeGeog.Rmd: tables with counts and percentages of population vaccinated  

## DialGraphics  
This folder contains all the images of the CDPHE Dial Framework that were included in the Routt County dashboard at various points in time.  
- Dial-Level-Green.jpg
- Dial-Level-Orange.jpg
- Dial-Level-Purple.jpg
- Dial-Level-Red.jpg
- Dial-Level-Yellow.jpg
- Dial-Level Blue.jpg

## Images
This folder contains all non-CDPHE Dial Framework images used on the dashboard.  
- RouttPHlogo  

## R Scrips
This folder contains all the R scripts used for data import, cleaning, and initial analysis. These scripts are sourced by Rmd files to prepare all data used in the dashboard.  
 - CalendarTable.R: creates a calendar table for reference  
 - CompareELR.R: used to diagnose and troubleshoot differences in two ELR source files  
 - DataExploration.R: used for various data exploration  
 - DrJCEDRSvaxQA.R: joins data from CEDRS and Dr. Justina case investigation software, generates mismatch and records needing QA in state immunization registrty  
 - Hospitalizations.R: loads and cleans hospitalization data from locally maintained tracking and state hospitalizations data  
 - IncidencePositivyAgeSchoolCompare.R: compares incidence and positivity among school age cases for one-off analyses  
 - OLDTestResultsandCases_fortesting.R: script that uses CEDRS source file for cases for testing change to LPHA portal source file  
 - Outbreaks.R: loads, cleans, and prepares outbreak data  
 - person_time_analysis.R: performs person-time analysis of vaccine efficacy  
 - point_in_time_ve.R: performs point-in-time vaccine efficacy  
 - point_in_time_ve_rolling.: performs rolling point in time vaccine efficacy  
 - RouttPopTables.R: generates tables with Routt County population numbers  
 - SchoolsCaseReportForm.R: loads, cleans, and prepares data from Schools Case Reporting Form, sends school-specific sheets to each school  
 - SchoolsDrJData.R: loads, cleans, prepares data on school-associated cases from Dr. Justina case investigation software  
 - SchoolVaxRate.R: loads school rosters and vaccine registry data to calculate school-specific vaccination rates  
 - ShinyAppMetrics.R: loads ShinyApps api data and generates usage statistics  
 - Symptoms.R: loads, cleans, and prepares symptoms data from Dr. Justina
 - TestResultsandCases.R: main script to load, clean, and prepare all cases and test data mainly from CDPHE files  
 - TestsStatic.R: Static file, not in use after spring 2021 when all providers began reporting electronically  
 - VaxData.R: load, cleans, and prepares vaccination data  
 - WeeklyTrackingSheet.R: loads, cleans, and prepares data from locally-maintained case tracking files  
