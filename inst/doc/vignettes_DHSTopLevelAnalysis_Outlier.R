## ----load packages------------------------------------------------------------
library(DSIWastewater)

## ----create worksheet4--------------------------------------------------------
data(WasteWater_data, package = "DSIWastewater")

baseWaste_data <- buildWasteAnalysisDF(WasteWater_data)

#Only show Site with more than 180 measurements for vignette for brevity
baseWaste_data <- baseWaste_data[baseWaste_data$n >= 180,]


workset4_Smooth_data <- do.call(rbind,
                              lapply(
                                split(baseWaste_data, ~WWTP),
                                LoessSmoothMod))

## ----flag outlier-------------------------------------------------------------
filter_outliers <- function(df, n){
  df_data <- computeJumps(df)
  ranked_data <- rankJumps(df_data)
  ranked_quantile_data <- computeRankQuantiles(ranked_data)
  classied_data <- flagOutliers(ranked_quantile_data, n)
  created_data <- RemoveOutliers(classied_data)
  return(created_data)
}

## -----------------------------------------------------------------------------
created_data <- filter_outliers(workset4_Smooth_data, 9)

## ----run regression analysis--------------------------------------------------
reg_estimates_data <- buildRegressionEstimateTable(created_data,
                               RunOn = c("sars_cov2_adj_load_log10",
                                "sars_adj_log10_Filtered",
                                "Loess"))
head(reg_estimates_data)

## ----Compare methods,fig.width=6, eval = FALSE--------------------------------
#  
#  createConfMatrix_Plot(reg_estimates_data,
#                        "sars_cov2_adj_load_log10",
#                        "Loess")
#  createConfMatrix_Plot(reg_estimates_data,
#                        "Loess",
#                        "sars_adj_log10_Filtered")
#  
#  createMethodCompareBar_Plot(reg_estimates_data)

## ----make DHS plot, fig.height=15,fig.width=14.25, warning = FALSE------------
createRegressionAnalysis_Plot(reg_estimates_data, created_data, 
                 PointVal = c( "sars_cov2_adj_load_log10",
                               "sars_adj_log10_Filtered"),
                 LineVal = "Loess")

