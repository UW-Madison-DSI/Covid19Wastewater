#' classifyRegressionAnalysis
#' 
#' Adds the DHS Classification scheme to data created by runRegressionAnalysis
#'
#' @param DF dataframe that contains results of buildRegressionEstimateTable
#' @param PSigTest Controls if we filter values with P-values>.3
#'
#' @export
#' @return DF with an extra column Catagory containing the results of the DHS binning
#' @example 
#' data(example_data, package = "DSIWastewater")
#' example_data$modeled_percentchange = 0
#' example_data$lmreg_sig = .01
#' classifyRegressionAnalysis(example_data)
classifyRegressionAnalysis <- function(DF, PSigTest=TRUE){
#magic variables: modeled_percentchange, lmreg_sig
  
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


#' Create Case Flags based on regression slope
#'
#' @param DF dataframe that contains results of buildRegressionEstimateTable
#' @param slopeThreshold number threshold for case_flag flagging
#' @param minSize case threshold for case_flag_plus_comm.threshold flagging
#'
#' @return DF with an three extra column Category containing the case flags
#' case_flag: when the 7 day slope is above slopeThreshold
#' case_flag_plus_comm.threshold: when case flag and more then 200 cases
#' slope_switch_flag: the first case flags in consecutive case flags
#' @export
#' @example 
#' example_DF <- data.frame(site = "madison", lmreg_slope = 5, value = 300)
#' classifyCaseRegression(example_DF)
classifyCaseRegression(example_DF)
classifyCaseRegression <- function(DF, slopeThreshold = 5, minSize = 200){
  RetDF <- DF%>%
    group_by(site)%>%
    mutate(
      #A flag when the slope for most recent week is greater than slopeThreshold/100k/day
      case_flag = case_when(lmreg_slope > slopeThreshold ~ 1,
                            TRUE ~ 0),
      
      #A flag when the previous slope and the signal is above 200
      case_flag_plus_comm.threshold = case_when(case_flag == 1 
                                                & value > 200 ~ 1,
                                                TRUE ~ 0),
      
      #What about a case flag where slope shifts from <5 to >5
      slope_switch_flag = case_when(lag(case_flag, 1) == 0 & 
                                      case_flag == 1 ~ 1,
                                    TRUE ~ 0))
  return(RetDF)
}

#' Classify FlagRegression  with rolling Quantile info 
#' 
#' Create wastewater flags based on the CDC classification defined in 
#' classifyRegressionAnalysis and the quantile rank of the date.
#'
#' @param DF dataframe that contains results of buildRegressionEstimateTable and
#' makeQuantileColumns
#' @param Pval threashold needed for flag_ntile_pval to flag 
#'
#' @export
#' @return DF with three extra columns 
#' cdc_flag: when the CDC method labels as 'major increase'
#' flag_ntile: when the cdc flag and its in the top quantile
#' flag_ntile_pval: when the flag ntile and the regression slope is less
#' than pval
#' @example 
#' data(example_data, package = "DSIWastewater")
#' example_data$modeled_percentchange = 0
#' example_data$lmreg_sig = .01
#' example_data$pastKavg.wwlog10 = 5
#' example_data$ntile = 8
#' classifyQuantileFlagRegression(example_data)
classifyQuantileFlagRegression <- function(DF, pval = .3){
  #Get the DHS Classification scheme of wastewater concentration
  Classification_DF <- classifyRegressionAnalysis(DF, PSigTest = FALSE)
  #returned DF piped into four mutate calls to add three columns
  Ret_DF <- Classification_DF%>%
    #Convert the classification scheme from the cdc into a flag
    mutate(cdc_flag = case_when(Catagory == "major increase"~ 1,
                                TRUE ~ 0))%>%
    #select only the flags that are on days that are larger then quantile
    mutate(flag_ntile = case_when(
             pastKavg.wwlog10 > ntile & cdc_flag ~ 1,
             TRUE ~ 0))%>%
    #further select so that the slope of the regression is less then pval
    mutate(flag_ntile_pval = case_when(
             flag_ntile & lmreg_sig < pval ~ 1,
             TRUE ~ 0))%>%
    #make NA into 0 or FALSE
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))
  return(Ret_DF)
}
