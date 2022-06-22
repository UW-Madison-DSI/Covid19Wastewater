## ----load packages------------------------------------------------------------
library(DSIWasteWater)
library(dplyr)

## ----create worksheet4--------------------------------------------------------
#Should add more info about the data
data(Data_wastewater, package = "DSIWasteWater")

workset4 <- BuildWorkSheet4(Data_wastewater)

#Only show Site with 150 measurements for vignette 
workset4 <- workset4%>% 
  filter(n >= 150)

## ----run package code---------------------------------------------------------

reg_estimates <- BuildRegressionEstimateTable(workset4)
head(reg_estimates)

## ----make DHS plot, fig.height=5,fig.width=78---------------------------------
#comment explaining why we need two dataframes
#Pick better name for plot function
DHSTopLevelPlots(reg_estimates, workset4)


