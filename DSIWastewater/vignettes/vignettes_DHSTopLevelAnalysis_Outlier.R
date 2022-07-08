## ----load packages------------------------------------------------------------
library(DSIWastewater)

## ----create worksheet4--------------------------------------------------------
data(wastewater_data, package = "DSIWastewater")

workset4_data <- buildWorkSheet4(wastewater_data)

#Only show Site with more than 180 measurements for vignette for brevity
workset4_data <- workset4_data[workset4_data$n >= 180,]


workset4_Smooth_data <- do.call(rbind,
                              lapply(
                                split(workset4_data,~WWTP),
                                LoessSmoothMod))


## ----flag outlier-------------------------------------------------------------

df_data <- computeJumps(workset4_Smooth_data)
ranked_data <- rankJumps(df_data)
ranked_quantile_data <- computeRankQuantiles(ranked_data)
classied_data <- flagOutliers(ranked_quantile_data, 9)
created_data <- RemoveOutliers(classied_data)

