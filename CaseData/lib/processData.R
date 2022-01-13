## read and format data -----------------------------------

#############################################
##  fetch and reformat daily DHS data;
getDHS_Data <- function(dataSource) {
  fields <- c("GEOID","DATE","POSITIVE", "NEGATIVE")  
  dhsData <-
    read.csv(dataSource,stringsAsFactors = FALSE) %>% 
    rename(POSITIVE = POS_CP,
           NEGATIVE = NEG,
           DATE = Date)%>%
    ## testing hook
    #write.csv(dhsData1,"data/raw/dhsData.2020-01-04.csv",
    #          quote = FALSE, row.names = FALSE)
    select(all_of(fields))  %>% 
    #filter(GEO == "County" | GEO == "State") %>% 
    mutate(DATE = mdy((.$DATE))) %>%
    rename(Date = "DATE") %>% 
    mutate(POSITIVE = sub(-999,0,POSITIVE)) %>% 
    mutate(NEGATIVE = sub(-999,0,NEGATIVE))
  
    ## enforce monotonicity in positive and negative test results;
    dhsData <-  dhsData %>% 
      group_by(GEOID) %>% 
      arrange(desc(Date)) %>% 
      mutate(NEGATIVE = cummin(NEGATIVE)) %>% 
      mutate(POSITIVE = cummin(POSITIVE)) %>% 
      mutate(Tests = NEGATIVE + POSITIVE) %>% 
      rename(Cases = "POSITIVE") %>% 
      select(-c("NEGATIVE")) %>% 
      arrange(Date)

    return(dhsData)
} ## getDHS_Data

## Aggregate cases by sewage service areas
getCasesForServiceAreas <- function(dhsData,serviceAreas){
  ServiceIDs <- unique(serviceAreas$ServiceID)
  casesTimeSeries <- lapply(ServiceIDs, function(ID) {
    censusTracts <- serviceAreas %>% 
      filter(ServiceID == ID) %>% 
      select(GEOID) 
    cases <- dhsData %>% 
      filter(GEOID %in% censusTracts[,1]) %>% 
      group_by(Date) %>% 
      summarize(
        Tests = sum(Tests),
        Cases = sum(Cases),
        Population = sum(Population),
        .groups = 'drop') %>% 
      mutate(ServiceID = ID)
    return(cases)
  }
  
  )  ## lapply
  casesTimeSeries <- do.call("rbind",casesTimeSeries)
  
  # Aggregate all census tracts
  MMSD_Cases <- dhsData %>% 
    filter(GEOID %in% serviceAreas$GEOID) %>% 
    group_by(Date) %>% 
    summarize(Cases = sum(Cases), 
              Tests = sum(Tests),
              Population = sum(Population),
              .groups = 'drop') %>% 
    mutate(ServiceID = "MMSD")
  casesTimeSeries <- rbind(MMSD_Cases,casesTimeSeries)
  return(casesTimeSeries)
}  ## function getCasesForServiceAreas()

### Refactor the functions below:

#############################################
## get population data
## needs to be modified for census tract level estimates
getCensusData <- function(censusFile) {
  censusData <- read_excel(censusFile)
  censusData <- 
    censusData %>% 
    rename(Population = "POP10") %>%
    select(GEOID, Population)  

    return(censusData)
}


#############################################
##  process the data to prep for plotting;
analyzeData <- function(dhsData,lag) {
    casesData <-  dhsData %>% 
        mutate(newCases = Cases - lag(Cases, n=lag,default = NA)) %>% 
        mutate(newTests = Tests - lag(Tests,n = lag, default = NA)) %>% 
        mutate(posFraction = newCases/newTests) %>% 
        mutate(newCases.per1000 = newCases/Population*1000/lag ) %>% 
        mutate(newTests.per1000 = newTests/Population*1000/lag )
    
    casesData <- casesData %>% 
        mutate(dailyPos = Cases - lag(Cases,n=1)) %>% 
        mutate(dailyTests = Tests - lag(Tests,n=1)) %>% 
        mutate(dailyFractionPos = dailyPos/dailyTests) %>% 
        mutate(Cases.per1000 = dailyPos/Population*1000) %>% 
        mutate(Tests.per1000 = dailyTests/Population*1000)

    return(casesData)
} ## analyzeData 

## analyzeDataByRegion
analyzeDataByRegion <- function(dhsData,lag,geoGrouping = "County") {
  ## first aggregate data by geoGrouping (DHS Region or herc_region)
  ## then hand off to analyzedData()
    if (geoGrouping == "County") {
        return(analyzeData(dshData,lag))
    }
  d <- dhsData %>% 
    group_by(!!sym(geoGrouping),Date) %>% 
    summarize_at(vars("Cases","Tests","Population"), sum) %>% 
    mutate(County = !!sym(geoGrouping))

  return(analyzeData(d,lag))
} ## analyzeDataByRegion

