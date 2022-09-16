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

