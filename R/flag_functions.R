#' Title
#'
#' @param DF 
#'
#' @return
#' @export
#'
#' @examples
CreateCaseFlag <- function(DF){
  
  Case_DF <- buildCaseAnalysisDF(DF)
  
  CaseRegressionOutput <- buildRegressionEstimateTable(DataMod = Case_DF,
                                                       RunOn = "FirstConfirmed.Per100K",
                                                       SplitOn = "Site",
                                                       DaysRegressed = 7,
                                                       PSigTest = FALSE)
  
  CaseFlagOutput <- ClassifyCaseRegression(CaseRegressionOutput)
  
  names(CaseFlagOutput)[names(CaseFlagOutput) == 'Site'] <- 'WWTP'
  
  CaseFlags <- CaseFlagOutput[,c("WWTP", "date", "case_flag",
                                 "case_flag_plus_comm.threshold",
                                 "slope_switch_flag")]
  return(CaseFlags)
}


#' Title
#'
#' @param DF 
#' @param windows 
#' @param quants 
#'
#' @return
#' @export
#'
#' @examples
CreateWasteFlags <- function(DF,
                             windows = c(14, 30, 60 , 90),
                             quants = c(5:9)/10){
  
  baseWaste_DF <-  buildWasteAnalysisDF(DF)
  
  Quantiles_DF <- MakeQuantileColumns(baseWaste_DF,
                                      quants, windows,
                                      "sars_cov2_adj_load_log10")
  
  Quantiles_DF <- Quantiles_DF[,c("WWTP", "date",
                                  "pastKavg.wwlog10",
                                  "window", "quant", "ntile")]
  
  CDCMethod <- buildRegressionEstimateTable(baseWaste_DF, 
                                            PSigTest=FALSE)
  
  CDCMethod <- CDCMethod[,c("WWTP", "date",
                            "lmreg_sig",  "Catagory",
                            "modeled_percentchange")]
  
  FULL_reg_DF <- dplyr::full_join(Quantiles_DF, CDCMethod,
                                  by = c("WWTP", "date"))
  
  FULL_reg_DF <- ClassifyQuantileFlagRegression(FULL_reg_DF)
  
  FULL_reg_DF$WWTP <- ifelse(FULL_reg_DF$WWTP == "Madison MSD WWTF",
                             "Madison",
                             FULL_reg_DF$WWTP)
  
  Full_wasteFlags <- FULL_reg_DF[,c("WWTP", "date", "window",
                                    "quant", "cdc_flag", "flag_ntile",
                                    "flag_ntile_pval")]
  return(Full_wasteFlags)
}