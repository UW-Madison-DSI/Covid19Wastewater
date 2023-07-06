## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----load and show data-------------------------------------------------------
library(DSIWastewater)
data(WasteWater_data, package = "DSIWastewater")
head(buildWasteAnalysisDF(WasteWater_data))

## ----explain all / most relavent functions------------------------------------
data(Case_data, package = "DSIWastewater")
data("pop_data", package = "DSIWastewater")#need to fix pop_data
#head(buildCaseAnalysisDF(left_join(Case_data, pop_data, by = "site")))


## ----explain / show use case and link to other vignettes----------------------
#TODO work on comparing the output of these methods 

