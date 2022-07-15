#' FCVLM
#'
#'fit n-1 models droping 1 data point
#'returns the model with the lowest p-val form among the,
#'
#' @param Formula LM model to be fit on robust data
#' @param data Contains the data needed for Formula to work
#'
#' @return the best of the lm models by p values
FCVLM <- function(Formula, data){
  lenDF <- nrow(data)
  LMOptions <- lapply(1:(lenDF+1), LMDropList, Formula, data)%>%
    LMSelectBestList()
}

#' LMDropList
#' 
#' fits a lm model with the nth row of data removed
#'
#' @param n the index of the row to drow
#' @param Formula LM model to be fit on robust data
#' @param data Contains the data needed for Formula to work
#'
#' @return a lm object
LMDropList <- function(n, Formula, data){
  ReducedDF <- data[-n,] 
  return(lm(Formula, data = ReducedDF))
}

#' LMSelectBestList
#' 
#'
#' @param LMList List of possible linear models
#' @param Verbose prints the row removed and the p value
#'
#' @return the LM with the lowest P-value
LMSelectBestList <- function(LMList, Verbose = FALSE){
  BestPVal <- 1
  RetLM <- LMList[[1]]
  for(i in 1:length(LMList)){
    ModPVal <- summary(LMList[[i]])$coefficients[2,4]
    if(is.nan(ModPVal)){next}
    if(Verbose){
      print(paste(BestPVal, ModPVal))
    }
    if(BestPVal > ModPVal){
      BestPVal <- ModPVal
      RetLM <- LMList[[i]]
    }
  }
  return(RetLM)
}