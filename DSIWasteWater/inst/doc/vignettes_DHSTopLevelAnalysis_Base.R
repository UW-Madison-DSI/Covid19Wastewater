## ----load packages------------------------------------------------------------
library(DSIWasteWater)

## ----create worksheet4--------------------------------------------------------
data(wastewater_data, package = "DSIWasteWater")

workset4_data <- buildWorkSheet4(wastewater_data)

#Only show Site with more than 180 measurements for vignette for brevity
workset4_data <- workset4_data[workset4_data$n >= 180,]

## ----run regression analysis--------------------------------------------------
reg_estimates_data <- buildRegressionEstimateTable(workset4_data)
head(reg_estimates_data)

## ----make DHS plot, fig.height=5,fig.width=38---------------------------------
createDHSMethod_Plot(reg_estimates_data, workset4_data)

