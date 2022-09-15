#' Create Case flags
#' 
#'Create case flags created by the DHS to warn of increasing cases
#' 
#' @param DF Input data frame
#'
#' @return data frame with columns:
#' Site: Location of flag
#' date: date of flag
#' case_flag: when the 7 day slope is above 5
#' case_flag_plus_comm.threshold: when case flag and more then 200 cases
#' slope_switch_flag: the first case flags in consecutive case flags
#' @export
CreateCaseFlag <- function(DF){
  
  #Prep case data into form for buildRegressionEstimateTable
  Case_DF <- buildCaseAnalysisDF(DF)
  
  #run 7 day rolling regression on FirstConfirmed.Per100K column  
  CaseRegressionOutput <- buildRegressionEstimateTable(DataMod = Case_DF,
                                                       RunOn = "FirstConfirmed.Per100K",
                                                       SplitOn = "Site",
                                                       DaysRegressed = 7,
                                                       PSigTest = FALSE)
  
  #Classify slope to create 3 flags described in @return
  CaseFlagOutput <- ClassifyCaseRegression(CaseRegressionOutput)
  
  #return only flags and type columns
  CaseFlags <- CaseFlagOutput[,c("Site", "date", "case_flag",
                                 "case_flag_plus_comm.threshold",
                                 "slope_switch_flag")]
  return(CaseFlags)
}


#' Create waste flags
#' 
#'Create waste flags created by the DHS and the CDC to warn of increasing cases
#' 
#'
#' @param DF Input data frame
#' @param windows what windows for the rolling quantile to use
#' @param quants what quantile for the rolling quantile to use 
#'
#' @return data frame with columns:
#' Site: Location of flag
#' date: date of flag
#' window: what window the quantile is from
#' quant: what quantile the quantile is from
#' cdc_flag: flag if the linear regression of the last 5 days estimates a 
#'      total percent change from the start of the period to the end of above
#'      100
#' flag_ntile: if there is a cdc flag and the measurement is above the last
#'      window days quant quantile
#' flag_ntile_pval: if there is a flag_ntile and the p value of the regression
#'      is less then .3
#' @export
#' @keywords internal
CreateWasteFlags <- function(DF,
                             windows = c(14, 30, 60 , 90),
                             quants = c(5:9)/10){
  #get DF into format for buildRegressionEstimateTable
  baseWaste_DF <-  buildWasteAnalysisDF(DF)
  
  #add quantile data to merge with the regression results
  Quantiles_DF <- MakeQuantileColumns(baseWaste_DF,
                                      quants, windows,
                                      "sars_cov2_adj_load_log10")
  
  #Get 5 day rolling regression of data
  CDCMethod <- buildRegressionEstimateTable(baseWaste_DF, 
                                            PSigTest=FALSE)
  #merge the regression DF and the quantile DF to get info for 
  #ClassifyQuantileFlagRegression
  FULL_reg_DF <- dplyr::full_join(Quantiles_DF, CDCMethod,
                                  by = c("WWTP", "date"))
  
  #create flags described in @return
  FULL_reg_DF <- ClassifyQuantileFlagRegression(FULL_reg_DF)
  
  #rename Madison name to make merging it with cases easier
  FULL_reg_DF$WWTP <- ifelse(FULL_reg_DF$WWTP == "Madison MSD WWTF",
                             "Madison",
                             FULL_reg_DF$WWTP)
  
  #Rename WWTP to Site so it matches the case flag DF label
  names(FULL_reg_DF)[names(FULL_reg_DF) == 'WWTP'] <- 'Site'
  
  #return only flags and type columns 
  Full_wasteFlags <- FULL_reg_DF[,c("Site", "date", "window",
                                    "quant", "cdc_flag", "flag_ntile",
                                    "flag_ntile_pval")]
  return(Full_wasteFlags)
}