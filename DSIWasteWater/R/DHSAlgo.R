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
#' @param n 
#' @param Formula LM model to be fit on robust data
#' @param data Contains the data needed for Formula to work
#'
#' @return
#' @export
#'
#' @examples
LMDropList <- function(n, Formula, data){
  ReducedDF <- data[-n,] 
  return(lm(Formula, data = ReducedDF))
}

#' LMSelectBestList
#' 
#'
#' @param LMList List of posible linear models
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


#' DHSInnerLoop
#' 
#' Runs a Linear regression on the data and returns it in a form to be merged
#' in the DHSOuterLoop function
#'
#' @param Formula LM model to be fit on DF
#' @param DF Contains the data needed for Formula to work
#' @param LMMethod Controls what Linear model is applied. 
#' intended options are lm and FCVLM
#'
#' @return a row of a DF containing the 
#' WWTP, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
DHSInnerLoop <- function(Formula, DF, LMMethod = lm){
  IndiVar <- as.character(Formula)[2]
  
  
  if(length(na.omit(pull(DF,IndiVar))) < 2){#The lm call will fail with 1 row
    reg_estimates = DF %>%
      
      filter(date == max(date)) %>%
      
      select(WWTP, date) %>%
      
      mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
             lmreg_n = nrow(filter(DF, !is.na(!!sym(IndiVar)))),
             lmreg_slope = NA,
             lmreg_sig = NA,
             modeled_percentchange = NA,
             Method = IndiVar
             )
    return(reg_estimates)
  }
  
  lm.subset.sum <- suppressWarnings(#data is sometimes functionary linear.
    summary(LMMethod(Formula, data = DF))
  )
  
  # Extract row to bind with workset
  ww.x.tobind <- DF %>%
    
    filter(date == max(date)) %>%
    
      select(WWTP, date) %>%
    
      mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
             
             lmreg_n = nrow(DF),
             
             lmreg_slope = lm.subset.sum$coefficients[2,1],
             
             lmreg_sig = lm.subset.sum$coefficients[2,4],
             
             modeled_percentchange = ((10^(lmreg_slope*days_elapsed))-1)*100,
             
             Method = IndiVar)

  return(ww.x.tobind)
}

#' DHSOuterLoop
#'
#' The DHS model system. runs a LM on each each set of n consecutive measurements
#' and returns a summary of the information
#'
#'
#' @param DF Contains TS data should only contain one WWTP
#' @param Formula LM model to be fit on a subsection of the data
#' @param n number of rows to be in each LM regression
#' @param LMMethod Controls what Linear model is applied. 
#' intended options are lm and FCVLM
#' @param verbose prints what site we are in
#' @export
#' @return a row of a DF containing the 
#' WWTP, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
DHSOuterLoop <- function(DF,Formulas,n = 5,LMMethod=lm, verbose = FALSE){
  
  reg_estimates = as.data.frame(matrix(ncol=9, nrow=0))
  
  colnames(reg_estimates) = c("WWTP", "date", "days_elapsed", "lmreg_n" , 
                              "lmreg_slope", "lmreg_sig", "modeled_percentchange", "Method", "LMmethod")
    
    if(verbose){
      print(unique(DF$WWTP)[[1]])
    }
    
    for (k in 1:(nrow(DF) - n)){
      
        ww.x.subset = DF[c(k:(k+n)),]
        
        ww.x.tobind = Formulas%>%
          lapply(DHSInnerLoop,
                 DF = ww.x.subset,
                 LMMethod = LMMethod)%>%
          bind_rows()
        
        reg_estimates <- rbind(reg_estimates, ww.x.tobind)
    }
  
  return(reg_estimates)
}


#' DHSClassificationFunc
#' 
#' Adds the DHS Classification scheme to data created by DHSOuterLoop
#'
#' @param DF 
#' @param PSigTest Controls if we filter values with P-values>.3
#' @export
#' @return
DHSClassificationFunc <- function(DF, PSigTest=TRUE){

  
  RetDF <- DF%>%
    mutate(Catagory = cut(modeled_percentchange, c(-Inf,-100,-10,10,100,Inf),
                          ordered_result=TRUE),
           Catagory = as.numeric(Catagory))
  
  if(PSigTest){
    RetDF <- RetDF%>%
      mutate(Catagory = ifelse(lmreg_sig>.3, "no change", Catagory))
    levl <- c(1,2,3,"no change",4,5)
    Catagorylabel = c("major decrease", "moderate decrease",
                      "fluctuating", "no change",
                      "moderate increase", "major increase")
  }else{
    levl <- c(1,2,3,4,5)
    Catagorylabel = c("major decrease", "moderate decrease",
                      "fluctuating", 
                      "moderate increase", "major increase")
  }
  
  RetDF <- RetDF%>%
    mutate(Catagory = factor(Catagory, levels = levl, 
                             labels =  Catagorylabel))
  return(RetDF)
}