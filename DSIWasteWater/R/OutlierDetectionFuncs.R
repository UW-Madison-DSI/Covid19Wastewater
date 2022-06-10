#' wrapper for TrendSDOutlierDetec that stores outlier results
#' 
#' in the data frame. The input assumes the data is from one TS
#'
#' @param DF The DF Needs to have the Time label to be Date
#' @param VecName String name of the variable we are looking at outliers for
#' @param SDDeg How many Standard deviations away from the trend to flag
#' @param DaySmoothed Days used to get Standard deviation
#' @param n Number of iterations of method done
#' @param TrendFunc What function that is used to generate the trend
#' @param verbose Controls if it prints the % of data that is outliers
#' @param outCol What the name of the outVec is
#'
#' @return DF With col saying whether the method was flagged
#' @export
#'
#' @examples 

TrendSDOutlierFilter <- function(DF,VecName,SDDeg,DaySmoothed, outCol = "FlaggedOutlier",
                                 n = 5, TrendFunc = LoessSmoothMod, verbose=FALSE){
  ArangeDF <- DF%>%
    arrange(Date)
  
  DetectedOutliers <- TrendSDOutlierDetec(DF = ArangeDF, 
                                          VecName = VecName, 
                                          SDDeg = SDDeg, 
                                          DaySmoothed = DaySmoothed, 
                                          n = n, 
                                          TrendFunc = TrendFunc)
  
  if(verbose){
    print(paste( round( mean(DetectedOutliers)*100, digits  = 2),
                 ArangeDF$Site[1]))
    
    if(mean(DetectedOutliers)>.99){
      print("Warning most of the data was removed")
    }
  }
  
  ArangeDF[[outCol]] <- DetectedOutliers
  
  return(ArangeDF)
}


#' Decide outliers using calculated trend and standard deviation
#' 
#' Need to be Done
#'
#' @param DF The DF Needs to have the Time label to be Date Needs to be sorted by 
#' Date before called
#' @param VecName String name of the variable we are looking at outliers for
#' @param SDDeg How many Standard deviations away from the trend to flag
#' @param DaySmoothed Days used to get Standard deviation
#' @param n Number of iterations of method done
#' @param TrendFunc What function that is used to generate the trend
#'
#' @return Boolean of each row being an outlier
#' @export
#'
#' @examples 
TrendSDOutlierDetec <- function(DF,VecName,SDDeg,DaySmoothed=36,n = 5,
                                TrendFunc = LoessSmoothMod){
  FullDateRange <- data.frame(Date=seq(min(DF$Date, na.rm = TRUE),
                                       max(DF$Date, na.rm = TRUE),by ="day"))
  
  BestVectorDF <- DF%>%
    #full_join(FullDateRange, by="Date")%>%
    mutate(UsedVar = log1p(!!sym(VecName)))
  
  
  
  for(i in 1:n){#robustly remove outliers and recalc smooth line
    span = 2*ParameterGuess(DF, VecName, 17.8, .6)#Preset for loess span guess
    BestVectorDF <- BestVectorDF%>%
      TrendFunc("UsedVar", "Temp", span = span)%>%
      mutate(SD = rollapply(UsedVar - Temp, DaySmoothed, sd, na.rm=TRUE, partial=TRUE),
             UsedVar = ifelse(UsedVar > Temp + SDDeg * SD, Temp, UsedVar),
             UsedVar = ifelse(UsedVar < Temp - 2 * SDDeg * SD, Temp, UsedVar))%>%
              #Extra fudge as we are less interested in errors from being to small
      select(-Temp)#Remove col to prevent contamination
  }
  
  BestVectorDF <- BestVectorDF
  
  booleanReturn <- abs(BestVectorDF$UsedVar-log1p(DF[[VecName]])) > 2
  booleanReturn[is.na(booleanReturn)] <- FALSE
  
  return(booleanReturn)
}