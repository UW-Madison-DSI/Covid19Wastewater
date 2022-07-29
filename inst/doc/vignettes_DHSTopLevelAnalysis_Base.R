## ----load packages------------------------------------------------------------
library(DSIWastewater)

## ----create worksheet4--------------------------------------------------------
data(WasteWater_data, package = "DSIWastewater")

baseWaste_data <- buildWasteAnalysisDF(WasteWater_data)

#Only show Site with more than 180 measurements for vignette for brevity
baseWaste_data <- baseWaste_data[baseWaste_data$n >= 180,]

unique(baseWaste_data$WWTP)

## ----run regression analysis--------------------------------------------------
reg_estimates_data <- buildRegressionEstimateTable(baseWaste_data)
head(reg_estimates_data)

## ----make DHS plot, fig.height=15,fig.width=14.25-----------------------------
createRegressionAnalysis_Plot(reg_estimates_data, baseWaste_data)

## ----Show Madison Version of The Data, fig.height=5, fig.width = 7.3----------
reg_estimates_Reduced_data <- reg_estimates_data[
                                reg_estimates_data$WWTP == "Madison MSD WWTF",
                                ]

workset4_Reduced_data <- baseWaste_data[
                              baseWaste_data$WWTP == "Madison MSD WWTF",
                              ]

createRegressionAnalysis_Plot(reg_estimates_Reduced_data, workset4_Reduced_data)

