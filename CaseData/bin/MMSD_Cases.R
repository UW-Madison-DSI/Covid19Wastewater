## Rscript --vanilla bin/MMSD_Cases.R -inFile data/raw/dhsData.2020-01-04.csv -outFile results/2021-01-07/MMSD_Cases.csv

## Setup ---------------
library(dplyr)
library(lubridate)
library(readxl)

source("lib/processData.R")

defaultArgs <- list (
  outFile =  "results/2022-01-12/MMSD_Cases.csv",
  inFile = NULL,        ## hook to by-pass download for functional tests
  censusFile = "data/raw/censustract-00-10.xlsx"    # 
  #lag = 7,              ## smoothing interval (days)
)

args <- defaultArgs#R.utils::commandArgs(trailingOnly = TRUE,
        #                     asValues = TRUE ,
        #                     defaults = defaultArgs)

#lag <- as.integer(args$lag)
dhsURL <- "C:/Users/marli/Downloads/COVID19-Historical-V2-TRCT.csv"
#https://dhsgis.page.link/v2EF
#censusURL <- "https://www2.census.gov/programs-surveys/popest/datasetgetDHS_Data/2010-2019/counties/totals/co-est2019-alldata.csv"
censusURL <- 
  "http://www2.census.gov/programs-surveys/decennial/tables/time-series/tract-change-00-10/censustract-00-10.xlsx"

## hooks for testing
if (!is.null(args$inFile)) {
  dataSource <- args$inFile
} else {
  dataSource <- dhsURL 
}

if (!is.null(args$censusFile)) {
  censusFile <- args$censusFile  ##"data/raw/censustract-00-10.xlsx"
} else {
  censusFile <- tempfile(fileext = ".xlsx")
  download.file(censusURL,censusFile)
}
  
### read and format data -----------------------------------
dhsData <- getDHS_Data(dataSource)

## service area
# from Song Gao's processing of shape files
serviceAreasFile <- "data/raw/serviceareas_ct_merge.csv"

serviceAreas <- 
  read.csv(serviceAreasFile,stringsAsFactors = FALSE) %>% 
  rename(GEOID = "ct") %>%
  select(c(GEOID,ServiceID))
  ## Note the mapping GEOID -> ServiceID is not 1 to 1.
censusData <- getCensusData(censusFile)

##########  process data ---------------
## Add population data to case counts
dhsData <- dhsData %>%  
  inner_join(censusData,by="GEOID")

casesForServiceAreas <- getCasesForServiceAreas(dhsData,serviceAreas)
## get population data
#censusData <- getCensusData(censusURL)

###  output csv and initailize plot -----------
if (!is.null(args$outFile)) {
  write.csv(casesForServiceAreas,args$outFile, quote = FALSE, row.names = FALSE)
}

q()

