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
DHSInnerLoop <- function(Formula, DF,Keep = NULL, LMMethod = lm){
  IndiVar <- as.character(Formula)[2]
  DepVar <- as.character(Formula)[3]
  
  reg_estimates <- DF %>%
    
    filter(date == max(date)) %>%
    
    select(all_of(Keep), date)%>%
    
    mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
           lmreg_n = nrow(filter(DF, !is.na(!!sym(IndiVar)))),
           lmreg_slope = NA,
           lmreg_sig = NA,
           modeled_percentchange = NA
    )
  
  
  if(length(na.omit(pull(DF,IndiVar))) < 2){#The lm call will fail with 1 row
    return(reg_estimates)
  }
  
  lm.subset.sum <- suppressWarnings(#data is sometimes functionary linear.
    summary(LMMethod(Formula, data = DF))
  )
  
  # Extract row to bind with workset
  ww.x.tobind <- reg_estimates%>%
    
      mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
             
             lmreg_n = nrow(DF),
             
             lmreg_slope = lm.subset.sum$coefficients[2,1],
             
             lmreg_sig = lm.subset.sum$coefficients[2,4],
             
            modeled_percentchange = ((10^(lmreg_slope*days_elapsed))-1)*100)

  return(ww.x.tobind)
}


#' Run DHS analysis at a top level
#'
#' @param DataMod The DF containing the col RunOn + date
#' @param RunOn The col names of the values we wish to run
#' @param SplitOn A category to separate to create independent TS data
#' @param verbose Bool on whether it should print out what group it is on
#' @param PSigTest When categorizing if it should reject high pVals
#'
#' @return A DF with the associated Date and DHS analysis
#' @export
#'
#' @examples
DHSTopLevelAnalysis <- function(DataMod, RunOn, SplitOn, 
                                verbose=FALSE, PSigTest=TRUE){
  reg_estimates = DataMod%>%
    pivot_longer(all_of(RunOn), names_to = "Method")%>%
    filter(!is.na(value))%>%
    group_by(across(all_of(c(SplitOn,"Method"))))%>%
    group_split()%>%
    lapply(DHSOuterLoop, 
           Formula = value ~ date, #Should Be Gen
           Keep  = c(SplitOn, "Method"), 
           verbose = verbose)%>%
    bind_rows()%>%
    DHSClassificationFunc(PSigTest=PSigTest)%>%
    filter(!is.na(Catagory))
  return(reg_estimates)
}

uniqueVal <- function(Col,DF){
  return(unique(DF[[Col]]))
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
DHSOuterLoop <- function(DF, Formula,Keep = NULL,n = 5,LMMethod=lm, verbose = FALSE){
  
  reg_estimates = as.data.frame(matrix(ncol=8+length(Keep), nrow=0))
  
  
  colnames(reg_estimates) = c(Keep, "date", "days_elapsed", "lmreg_n" , 
                              "lmreg_slope", "lmreg_sig", "modeled_percentchange", "Method", "LMmethod")
    if(verbose){
      Keep%>%
        lapply(uniqueVal,DF = DF)%>%
        paste()%>%
        print()
    }#as.character(Formula)[2]
  
    ModDF <- DF%>%
      arrange(date)
    
    for (k in 1:(nrow(ModDF) - n)){
      
        ww.x.subset = ModDF[c(k:(k+n)),]
        
        ww.x.tobind = DHSInnerLoop(
                Formula,
                 DF = ww.x.subset,
                 Keep = Keep,
                 LMMethod = LMMethod)
        
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