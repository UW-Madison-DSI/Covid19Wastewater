#' Run DHS analysis at a top level
#' 
#' buildRegressionEstimateTable is used to create a data frame that has the
#' predicted categorization as laid out by the DHS. For each RunOn var supplied
#' It uses the var to create a 5 day lm fit and uses the percent change to bin
#' the results into 5 categories, "major decrease", "moderate decrease",
#' "fluctuating", "moderate increase", and "major increase".
#' If the model P-value if over .3 the category is replaced with "no change"
#'
#' @param DataMod The DF containing the col RunOn + date
#' @param RunOn The col names of the values we wish to run
#' @param SplitOn A category to separate to create independent TS data
#' @param verbose Bool on whether it should print out what group it is on
#' @param PSigTest When categorizing if it should reject high pVals
#' @param DaysRegressed number of days used in each regression
#'
#' @return A DF with the associated Date and DHS analysis
#' @export
#'
#' @examples
#' data(example_data, package = "DSIWastewater")
#' buildRegressionEstimateTable(example_data)
#' 
buildRegressionEstimateTable <- function(DataMod, 
                                         RunOn = "sars_cov2_adj_load_log10",
                                         SplitOn = "WWTP",
                                         DaysRegressed = 5,
                                         verbose=FALSE, 
                                         PSigTest=TRUE){
  reg_estimates = DataMod%>%
    pivot_longer(all_of(RunOn), names_to = "Method")%>%
    filter(!is.na(value))%>%
    group_by(across(all_of(c(SplitOn,"Method"))))%>%
    group_split()%>%
    lapply(runRegressionAnalysis, 
           Formula = value ~ date, #Should Be Gen
           Keep  = c(SplitOn, "Method","value"), 
           n = DaysRegressed,
           verbose = verbose)%>%
    bind_rows()%>%
    ClassifyRegressionAnalysis(PSigTest=PSigTest)%>%
    filter(!is.na(Catagory))
  
  return(reg_estimates)
}

#' runRegressionAnalysis
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
#' @param Keep The col in the original DF to keep besides Date
#' @param verbose prints what site we are in
#'
#' @export
#' @return a row of a DF containing the 
#' WWTP, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
runRegressionAnalysis <- function(DF, 
                                  Formula,
                                  Keep = NULL,
                                  n = 5,
                                  LMMethod=lm,
                                  verbose = FALSE){
  
  
  reg_estimates = as.data.frame(matrix(ncol=8+length(Keep), nrow=0))
  
  
  colnames(reg_estimates) = c(Keep, "date", "days_elapsed", "lmreg_n" , 
                              "lmreg_slope", "lmreg_sig", "modeled_percentchange", "Method", "LMmethod")
  if(verbose){
    Keep%>%
      lapply(uniqueVal,DF = DF)%>%
      paste()%>%
      print()
  }
  
  ModDF <- DF%>%
    filter(!is.na(!!sym(as.character(Formula)[2])))%>%
    arrange(date)
  
  for (k in 1:(nrow(ModDF) - (n - 1))){
    
    ww.x.subset = ModDF[k:(k+(n - 1)),]
    
    ww.x.tobind = regressionInnerLoop(
      Formula,
      DF = ww.x.subset,
      Keep = Keep,
      LMMethod = LMMethod)
    
    reg_estimates <- rbind(reg_estimates, ww.x.tobind)
  }
  
  return(reg_estimates)
}

#' regressionInnerLoop
#' 
#' Runs a Linear regression on the data and returns it in a form to be merged
#' in the runRegressionAnalysis function
#'
#' @param Formula LM model to be fit on DF
#' @param DF Contains the data needed for Formula to work
#' @param LMMethod Controls what Linear model is applied. 
#' intended options are lm and FCVLM
#' @param Keep The col in the original DF to keep besides Date
#'
#' @return a row of a DF containing the 
#' WWTP, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
regressionInnerLoop <- function(Formula, DF, Keep = NULL, LMMethod = lm){
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

#' Find all unique values in the column selected
#'
#' @param Col Col being looked at
#' @param DF the DF containing Col
#'
#' @return a list of each unique col value
uniqueVal <- function(Col,DF){
  return(unique(DF[[Col]]))
}