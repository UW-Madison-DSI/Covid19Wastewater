MadisonCaseFN  <-  "MMSD_Cases_processed.csv"
LIMSFN <- "LIMSWasteData_2021-06-30_17-40.csv"

#Importing the Madison case data
LatCaseDF <- ParseData(MadisonCaseFN)%>% 
  filter(Site == "Madison")%>%
  select(Date, Site, Cases,Tests)

#Importing the Madison waste water data
LIMSFullDF <- ParseData(LIMSFN)%>%
  filter(Site == "Madison")%>%
  select(Date, Site,  N1, N1Error)

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
#Custom_color_scale
ColorRule <- scale_color_manual(values = c("N1" = "#F8766D",
                                           "Cases"="#00BFC4",
                                           "SLDCases"="#800080",
                                           "loessN1" = "#FFFF00"))