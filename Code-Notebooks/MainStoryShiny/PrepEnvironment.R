#GeneralCaseFN  <-  "MMSD_Cases_2022_01_12_processed.csv"
#LIMSFN <- "LIMSWasteData_01-25-21_01-05-22.csv"
LIMSFN <- "LIMSWasteData_02-09-22.csv"
MadisonCaseFN <- "MMSD_Interceptor_Cases_2_7_22.csv"
#Importing the Madison case data
LatCaseDF <- ParseData(MadisonCaseFN)
  #Importing the Madison waste water data
  LIMSFullDF <- ParseData(LIMSFN)%>%
    filter(Site %in% unique(LatCaseDF$Site))%>%
    select(Date, Site,  N1, N2,AVG)
  #joining the two data frames together
  FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))


MinMaxNormalization <- function(Vec,ExtreameValues =NA){#normalizes the data to range from 0 and 1
  if(!any(is.na(ExtreameValues))){
    normVec <- (Vec-ExtreameValues[1])/ExtreameValues[2]
  }else{
    normVec <- (Vec-min(Vec,na.rm=TRUE))/max(Vec,na.rm=TRUE)
  }
  return(normVec)
}

NoNa <- function(DF,...){#Removes NA from the reverent columns
  ColumnNames <- c(...)
  NoNaDF <- DF%>%
    filter(
      across(
        .cols = ColumnNames,
        .fns = ~ !is.na(.x))
    )
  return(NoNaDF)
}

FillNA <- function(DF,...){#Fills NA with previous values
  ColumnNames <- c(...)
  NoNaDF <- DF%>%
    fill(ColumnNames)
  return(NoNaDF)
}

MedianMean <- function(Vec){# take the mean drooping the highest and lowest values
  RemoveBorderPoints <- replace(Vec, c(which.min(Vec), which.max(Vec)), NA)
  MeanValue <- mean(RemoveBorderPoints, na.rm = TRUE)
  return(MeanValue)
}