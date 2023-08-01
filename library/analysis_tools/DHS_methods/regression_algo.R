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
#' library(dplyr)
#' data(Example_data, package = "DSIWastewater")
#' Example_log_data <- mutate(Example_data, log_geo_mean = log10(geo_mean + 1))
#' head(buildRegressionEstimateTable(Example_log_data, SplitOn = "site", 
#'                                            RunOn = "log_geo_mean"))
buildRegressionEstimateTable <- function(DataMod, 
                                         RunOn = "sars_cov2_adj_load_log10",
                                         SplitOn = "site",
                                         DaysRegressed = 5,
                                         verbose = FALSE, 
                                         PSigTest = TRUE){
  reg_estimates = DataMod%>%
    pivot_longer(all_of(RunOn), names_to = "Method")%>%
    filter(!is.na(.data$value))%>%
    group_by(across(all_of(c(SplitOn,"Method"))))%>%
    group_split()%>%
    lapply(runRegressionAnalysis, 
           Formula = value ~ date, #Should Be Gen
           Keep  = c(SplitOn, "Method","value"), 
           n = DaysRegressed,
           verbose = verbose)%>%
    bind_rows()%>%
    classifyRegressionAnalysis(PSigTest=PSigTest)%>%
    filter(!is.na(.data$Catagory))
  
  return(reg_estimates)
}

#' runRegressionAnalysis
#'
#' The DHS model system. runs a LM on each each set of n consecutive measurements
#' and returns a summary of the information
#'
#'
#' @param DF Contains TS data should only contain one site
#' @param Formula LM model to be fit on a subsection of the data
#' @param n number of rows to be in each LM regression
#' @param Keep The col in the original DF to keep besides Date
#' @param verbose prints what site we are in
#' @keywords internal
#' @return a row of a DF containing the 
#' site, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
#' @examples 
#' data(Example_data, package = "DSIWastewater")
#' DSIWastewater:::runRegressionAnalysis(Example_data, Formula = geo_mean ~ date)
runRegressionAnalysis <- function(DF, 
                                  Formula,
                                  Keep = NULL,
                                  n = 5,
                                  verbose = FALSE){
  
  
  reg_estimates = as.data.frame(matrix(ncol=7+length(Keep), nrow=0))
  
  
  colnames(reg_estimates) = c(Keep, "date", "days_elapsed", "lmreg_n" , 
                              "lmreg_slope", "lmreg_sig", 
                              "modeled_percentchange", "Method")
  if(verbose){
    Keep[-length(Keep)]%>%
      lapply(uniqueVal, DF = DF)%>%
      paste()%>%
      print()
  }
  
  ModDF <- DF%>%
    filter(!is.na(!!sym(as.character(Formula)[2])))%>%
    arrange(date)
  
  for (k in 1:(nrow(ModDF) - (n - 1))){
    
    ww_x_subset = ModDF[k:(k+(n - 1)),]
    
    ww_x_tobind = regressionInnerLoop(
      Formula,
      DF = ww_x_subset,
      Keep = Keep)
    
    reg_estimates <- rbind(reg_estimates, ww_x_tobind)
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
#' @param Keep The col in the original DF to keep besides Date
#'
#' @return a row of a DF containing the 
#' site, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
#' @keywords internal
regressionInnerLoop <- function(Formula, DF, Keep = NULL){
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
  
  lm_subset_sum <- suppressWarnings(#data is sometimes functionary linear.
    summary(lm(Formula, data = DF))
  )
  
  # Extract row to bind with workset
  ww_x_tobind <- reg_estimates%>%
    
    mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
           
           lmreg_n = nrow(DF),
           
           lmreg_slope = lm_subset_sum$coefficients[2,1],
           
           lmreg_sig = lm_subset_sum$coefficients[2,4],
           
           modeled_percentchange = ((10^(.data$lmreg_slope * .data$days_elapsed))-1)*100)
  
  return(ww_x_tobind)
}

#' Find all unique values in the column selected
#'
#' @param Col Col being looked at
#' @param DF the DF containing Col
#'
#' @return a list of each unique col value
#' @keywords internal
#' @examples
#' data(Example_data, package = "DSIWastewater")
#' DSIWastewater:::uniqueVal("site", Example_data)
uniqueVal <- function(Col,DF){
  return(unique(DF[[Col]]))
}
