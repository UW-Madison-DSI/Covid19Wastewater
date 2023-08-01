#' Get a fitting parameter for the model
#'
#' @param DF The DF we are about to fit a smoothing curve on. should only
#' be from one TS
#' @param InVar The col name that the variable is about to smooth
#' 
#' @param Base The value we are scaling by the inverse of the number of
#' rows in the data
#' @param max The largest value returned
#'
#' @return a number between 0 and max
#' @keywords internal
#' @examples
#' data(Example_data, package = "DSIWastewater")
#' DSIWastewater:::parameterGuess(Example_data,"geo_mean", 17.8, .6)
parameterGuess <- function(DF,InVar, Base, max){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  span <- min(c(Base/temp$n, max))#More can be done here
  return(span)
}


#' loessSmoothMod
#' Add a column of the smoothed values using Loess
#'
#' @param DF DF we are adding the loess smooth col to
#' @param InVar The column to be smoothed
#' @param OutVar The name of the new column
#' @param span The span fed into loess smoothing. if it equals "guess" then it
#' if found using parameterGuess 
#' @param Filter Prefilter using the value of a Filter col
#'
#' @return A DF with an extra col with a loesss smoothed version of InVar
#' @export
#'
#' @examples
#' data(Example_data, package = "DSIWastewater")
#' head(loessSmoothMod(Example_data))
loessSmoothMod <- function(DF,InVar="N1",
                           OutVar="Loess", span="guess", Filter = NULL){
  if(span=="guess"){
    span <- parameterGuess(DF,InVar, 17.8, .6)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!(!!sym(Filter)))
  }
  
  DF <- DF%>%
    arrange(date)
  
  DF[[OutVar]] <- loessFit(y=(DF[[InVar]]), 
                            x=DF$date, #create loess fit of the data
                            span=span, 
                            iterations=2)$fitted#2 iterations remove some bad patterns
  
  if(!is.null(Filter)){
    DF <- DF%>%
      bind_rows(OutDF)
  }
  
  return(DF)
}



#' expSmoothMod
#' Add a column of the smoothed values using exponential smoothing
#' @param DF The DF we are to add a exponential smoothing column to
#' @param InVar The column to be smoothed
#' @param OutVar The name of the new column
#' @param alpha The alpha fed into forecast exponential smoothing. if it equals "guess" then it
#' if found using parameterGuess
#' @param beta  The beta fed into forecast exponential smoothing. if it equals "guess" then it
#' if found using parameterGuess
#' 
#' @param Filter Prefilter using the value of a Filter col
#'
#' @return A DF with an extra col with a exp smoothed version of InVar
#' @export
#' @examples
#' data("Example_data", package = "DSIWastewater")
#' Example_data <- Example_data[Example_data$site == "Green Bay",]
#' expSmoothMod(Example_data, "N1", "expN1")
expSmoothMod <- function(DF, InVar, OutVar, alpha="guess",beta="guess", Filter = NULL ){
  
  if(alpha=="guess"){
    alpha <- parameterGuess(DF,InVar, 35.6, .4)
  }
  if(beta=="guess"){
    beta <- parameterGuess(DF,InVar, 8.9, .4)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!(!!sym(Filter))  & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  DF <- DF%>%
    arrange(date)
  
  DF[[OutVar]] <- forecast::ets(y=DF[[InVar]],
                          model = "AAN",
                          alpha = alpha,
                          beta  = beta)$fitted
  DF <- DF%>%
    bind_rows(OutDF)
  
  return(DF)
}


#' nGuess for sgolaySmoothMod number of points per polynomial
#'
#' @param DF The DF to fit the sgolayfilt curve on. should only
#' be from one TS
#' @param InVar The col name that the variable is about to smooth
#' @param Base The value we are scaling by the of the number of
#' rows in the data
#' @param min The smallest value returned
#'
#' @return a number greater or equal to min
#' @keywords internal
#' @examples
#' data(Example_data, package = "DSIWastewater")
#' DSIWastewater:::nGuess(Example_data, "geo_mean", 50/178, 7)
nGuess <- function(DF,InVar, Base, min){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  N <- max(c(floor(Base*temp$n), min))#More can be done here
  return(N + 1 - N%%2)
}


#' sgolaySmoothMod
#' Add a column of the smoothed values using sgolayfilt
#' @param DF dataframe containing the columns specified below
#' @param InVar The column to be smoothed
#' @param OutVar The name of the new column
#' @param poly The degree of the polynomial fit
#' @param n The number of points per polynomial fed into sgolayfilt. 
#' if it equals "guess" then it is found using parameterGuess
#' @param Filter Prefilter using the value of a Filter col
#'
#' @return DF with an extra col with a sgolayfilt smoothed version of InVar
#' @export
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' DSIWastewater:::sgolaySmoothMod(WasteWater_data,"N1","sgolayN1")
sgolaySmoothMod <- function(DF,InVar, OutVar,poly=5,n="guess", Filter = NULL){
  if(n=="guess"){
    n <- nGuess(DF, InVar,50/178, 7)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    SmthDF <- DF%>%
      filter(!(!!sym(Filter)) & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    SmthDF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  SmthDF <- SmthDF%>%
    arrange(date)

  SmthDF[[OutVar]] <- SmthDF%>%
    pull(!!InVar)%>%
    signal::sgolayfilt(p  = poly, n = n)
  
  RetDF <- SmthDF%>%
    bind_rows(OutDF)%>%
    arrange(date)
  
  return(RetDF)
}


