## ----load packages------------------------------------------------------------
library(DSIWasteWater)
library(dplyr)

## ----create worksheet4--------------------------------------------------------
data(wastewater_data, package = "DSIWasteWater")

workset4_data <- buildWorkSheet4(wastewater_data)

#Only show Site with more than 180 measurements for vignette for brevity
workset4_data <- workset4_data[workset4_data$n >= 180,]

workset4_Smooth_data <- workset4_data%>%
  group_by(WWTP)%>%
  group_split()%>%
  lapply(LoessSmoothMod)%>%
  bind_rows()

## ----flag outlier-------------------------------------------------------------

df_data <- computeJumps(workset4_Smooth_data)
ranked_data <- rankJumps(df_data)
ranked_quantile_data <- computeRankQuantiles(ranked_data)
classied_data <- flagOutliers(ranked_quantile_data, 9)
created_data <- RemoveOutliers(classied_data)

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
createDHSMethod_Plot(reg_estimates_data, created_data, 
                 PointVal = c( "sars_cov2_adj_load_log10",
                               "sars_adj_log10_Filtered"),
                 LineVal = "Loess")

