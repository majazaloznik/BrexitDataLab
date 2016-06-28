###############################################################################
## "Data driven" analysis of Brexit asoociated results
###############################################################################
##
##
##
## interactive turnout-meter http://yougov.co.uk/turnout-o-meter/?turnout=71&overallremaing=4&agefactor=1&classfactor=0
###############################################################################
## 0. preliminaries
###############################################################################
require(curl)
require(jsonlite)
require(XLConnect)
require(tidyr)
require(dplyr)

## 1. DATA SOURCES 
###############################################################################
## 1.0 Actual results by 382 areas - .csv import
## 1.1. voter registration leveles by age group - manual input
## 1.2. last minute voter registration by age group - json import
## 1.3. voting turnout by age group - manual input
## 1.4. results by age group - Lord Ashcroft Polls - copy/paste pdf
## 1.5. UK population data by age  - ons, unzip, xls
## 1.5.1 UK life expectancy data  - ons, unzip, xls
## 2. save 7 tables
###############################################################################

## 1.0 Actual results by 382 areas - .csv import
###############################################################################
results.orig <- read.csv(paste("http://www.electoralcommission.org.uk/__data/assets/file/",
                  "0014/212135/EU-referendum-result-data.csv", sep=""))

###############################################################################
## 1.1. voter registration leveles by age group:
## Data from .pdf table http://www.electoralcommission.org.uk/__data/assets/pdf_file/0005/169889/Completeness-and-accuracy-of-the-2014-electoral-registers-in-Great-Britain.pdf
## The quality of the 2014 electoral registers in Great Britain 
## Manual input
## This data is from baseline registers before the individual electoral
## registration was implemented
## so feb and march 2014
## could be updated with registration data since then (only available June 14 on, see 2.)
###############################################################################
completeness.orig <- data.frame(
  age.group = c("18-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  registration.prop = c(76.1, 70.2, 73.8,85, 90.7,92.9, 95.4)/100
)

## 1.2. last minute voter registration by age group
## .json file from the register to vote service 
## actually this data is not useful at all..
## it tracks registrations by age group, but most of those
## are from people who are already registered, so no way of knowing
## how many are successful registrations..
## mentioned here: http://www.telegraph.co.uk/news/2016/06/10/eu-referendum-decision-to-extend-voter-registration-deadline-cou/
## where they are optimistic about the new young voters registring, but
## commit the error of assuming they will have a 75% turnout..
## ratio is about 3:2, data 2296530/ 1561093
## from 2015 election here:
## http://www.electoralcommission.org.uk/__data/assets/pdf_file/0006/190941/May-2015-polls-public-awareness-activity-report.pdf
###############################################################################
#registrations.all <- fromJSON("https://www.performance.service.gov.uk/data/register-to-vote/volumetrics?collect=count%3Asum&group_by=value&period=day&filter_by=metricName%3Aage_band&start_at=2014-03-01T00%3A00%3A00Z&end_at=2016-06-09T13%3A07%3A20Z")
# from 20.5.2016
registrations.orig <- fromJSON("https://www.performance.service.gov.uk/data/register-to-vote/volumetrics?collect=count%3Asum&group_by=value&period=day&filter_by=metricName%3Aage_band&start_at=2016-05-20T00%3A00%3A00Z&end_at=2016-06-09T13%3A07%3A20Z")

registrations.grouped.orig <- data.frame(age.group = registrations.orig$data$value,
                                         sum = registrations.orig$data$`count:sum`)
sum(registrations.grouped.orig$sum[3:10])
rm(registrations.orig)
## 1.3. voting turnout by age group
## yougov data from sky news tweet:https://twitter.com/SkyData/status/746700869656256512 
## manual input
###############################################################################
turnout.orig <- data.frame(
  age.group = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  turnout.prop = c(36, 58, 72, 75, 81, 83)/100
)

## 1.4.1 results by age group - Lord Ashcroft Polls - copy/paste pdf
## http://lordashcroftpolls.com/wp-content/uploads/2016/06/How-the-UK-voted-Full-tables-1.pdf
## weighted - 52:48 
###############################################################################
results.LA.orig <- data.frame(
  age.group = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  remain.prop = c(73, 62, 52, 44, 43, 40)/100
)


## 1.5. UK population data by age  - ons, unzip, xls
###############################################################################
# url of the .zip file we want
# data.zip.url <- paste("https://www.ons.gov.uk/file?uri=/peoplepopulationand",
#                       "community/populationandmigration/populationestimates/",
#                       "datasets/populationestimatesforukenglandandwalesscotl",
#                       "andandnorthernireland/mid2015/ukmye2015.zip", sep="")
# temp <- tempfile()
# download.file(data.zip.url, temp)
# # check what is in the zip file using list (doens't extract anything)
# unzip(temp, list=TRUE)

# # Only one file, that's the one we want to extract to the data folder
# unzip(temp, "MYE1_population_summary_for_UK.xls", exdir = "data")
# unlink(temp)
# rm(data.zip.url, temp)
UK.population.orig <- readWorksheetFromFile("data/MYE1_population_summary_for_UK.xls",
                            sheet=2,
                            startRow = 7,
                            endRow = 26,
                            endCol = 2)

# unzip(temp, "MYE2_population_by_sex_and_age_for_local_authorities_UK.xls", exdir = "data")
# unlink(temp)
# rm(data.zip.url, temp)
UK.population.orig <- readWorksheetFromFile("data/MYE2_population_by_sex_and_age_for_local_authorities_UK.xls",
                            sheet="UK persons",
                            startRow = 3,
                            endRow = 4, header=TRUE)



## 1.5.1 UK life expectancy data  - ons, unzip, xls
###############################################################################
# download.file(paste("http://www.ons.gov.uk/file?uri=/peoplepopulationand",
#                     "community/birthsdeathsandmarriages/lifeexpectancies/",
#                     "datasets/nationallifetablesunitedkingdomreferencetables",
#                     "/current/nltuk1214reg_tcm77-414438.xls", sep=""),
#               destfile= "data/lifeexpect.xls")

UK.life.exp.orig <- readWorksheetFromFile("data/lifeexpect.xls",
                                          sheet="2012-14",
                                          startRow = 7,
                                          endRow = 108,
                                          endCol=6,
                                          header=TRUE)

###############################################################################
## SAVE ALL ORIGINAL TABLES FOR IMPORT LATER
###############################################################################

save(results.orig, 
     completeness.orig,
     registrations.grouped.orig,
     turnout.orig, 
     results.LA.orig,
     UK.population.orig,
     UK.life.exp.orig, 
     file="prepared.data.Rdata")
  

