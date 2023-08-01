#' Create Case flags
#' 
#'Create case flags created by the DHS to warn of increasing cases
#' 
#' @param DF Input data frame
#'
#' @return data frame with columns:
#' site: Location of flag
#' date: date of flag
#' case_flag: when the 7 day slope is above 5
#' case_flag_plus_comm.threshold: when case flag and more then 200 cases
#' slope_switch_flag: the first case flags in consecutive case flags
#' @export
#' @examples
#' data(Case_data, package = "DSIWastewater")
#' data(pop_data, package = "DSIWastewater")
#' Case_data <- Case_data[Case_data$site == 'Algoma',]
#' createCaseFlag(dplyr::left_join(Case_data, pop_data))
createCaseFlag <- function(DF){
  
  #Prep case data into form for buildRegressionEstimateTable
  Case_DF <- buildCaseAnalysisDF(DF)
  
  #run 7 day rolling regression on FirstConfirmed.Per100K column  
  CaseRegressionOutput <- buildRegressionEstimateTable(DataMod = Case_DF,
                                                       RunOn = "FirstConfirmed.Per100K",
                                                       SplitOn = "site",
                                                       DaysRegressed = 7,
                                                       PSigTest = FALSE)
  
  #Classify slope to create 3 flags described in @return  
  CaseFlagOutput <- classifyCaseRegression(CaseRegressionOutput)
  
  #return only flags and type columns
  CaseFlags <- CaseFlagOutput[,c("site", "date", "case_flag",
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
#' site: Location of flag
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
#' @keywords internal
#' @examples 
#' data("WasteWater_data", package = "DSIWastewater")
#' data(pop_data, package = "DSIWastewater")
#' WasteWater_data <- WasteWater_data[WasteWater_data$site == "Waukesha",]
#' DSIWastewater:::createWasteFlags(dplyr::left_join(WasteWater_data, pop_data))
createWasteFlags <- function(DF,
                             windows = c(14, 30, 60 , 90),
                             quants = c(5:9)/10){
  #get DF into format for buildRegressionEstimateTable
  baseWaste_DF <-  buildWasteAnalysisDF(DF)
  
  #add quantile data to merge with the regression results
  Quantiles_DF <- makeQuantileColumns(baseWaste_DF,
                                      quants, windows,
                                      "sars_cov2_adj_load_log10")
  
  #Get 5 day rolling regression of data
  CDCMethod <- buildRegressionEstimateTable(baseWaste_DF, 
                                            PSigTest=FALSE)
  #merge the regression DF and the quantile DF to get info for 
  #classifyQuantileFlagRegression
  FULL_reg_DF <- dplyr::full_join(Quantiles_DF, CDCMethod,
                                  by = c("site", "date"))
  
  #create flags described in @return
  sars_cov2_adj_load_log10 <- NA
  FULL_reg_DF <- classifyQuantileFlagRegression(FULL_reg_DF, WW_column = sars_cov2_adj_load_log10)
  
  #rename Madison name to make merging it with cases easier
  FULL_reg_DF$WWTP <- ifelse(FULL_reg_DF$site == "Madison MSD WWTF",
                             "Madison",
                             FULL_reg_DF$site)
  
  #return only flags and type columns 
  Full_wasteFlags <- FULL_reg_DF[,c("site", "date", "window",
                                    "quant", "cdc_flag", "flag_ntile",
                                    "flag_ntile_Pval")]
  return(Full_wasteFlags)
}
