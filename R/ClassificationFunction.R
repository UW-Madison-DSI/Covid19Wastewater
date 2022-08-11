#' ClassifyRegressionAnalysis
#' 
#' Adds the DHS Classification scheme to data created by runRegressionAnalysis
#'
#' @param DF dataframe that contains results of buildRegressionEstimateTable
#' @param PSigTest Controls if we filter values with P-values>.3
#'
#' @export
#' @return DF with an extra column Catagory containing the results of the DHS binning
ClassifyRegressionAnalysis <- function(DF, PSigTest=TRUE){
  
  
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
#'
#' @return DF with an three extra column Catagory containing the case flags
#' @export
ClassifyCaseRegression <- function(DF){
  RetDF <- DF%>%
    mutate(
      #A flag when the slope for most recent week is greater than 5/100k/day
      case_flag = case_when(lmreg_slope > 5 ~ 1,
                            TRUE ~ 0),
      
      #A flag when the previous slope and the signal is above 200
      case_flag_plus_comm.threshold = case_when(case_flag == 1 
                                                & value > 200 ~ 1,
                                                TRUE ~ 0),
      
      #What about a case flag where slope shifts from <5 to >5
      slope_switch_flag = case_when(lag(lmreg_slope, 1) < 5 & 
                                      lmreg_slope > 5 ~ 1,
                                    TRUE ~ 0))
  return(RetDF)
}

#' Title
#'
#' @param DF 
#'
#' @return
#' @export
#'
#' @examples
ClassifyQuantileFlagRegression <- function(DF){
  RetDF <- DF%>%
    mutate(cdc_flag = case_when(Catagory == "major increase"~ 1,
                                TRUE ~ 0),
           flag_ntile = case_when(
             pastKavg.wwlog10 > ntile & cdc_flag ~ 1,
             TRUE ~ 0),
           flag_ntile_pval = case_when(
             flag_ntile & lmreg_sig < pval ~ 1,
             TRUE ~ 0))%>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x), 0, .x)))
  return(RetDF)
}