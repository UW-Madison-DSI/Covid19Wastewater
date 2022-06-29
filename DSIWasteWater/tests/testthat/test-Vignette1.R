test_that("Test vignettes DHSTopLevelAnalysis_Base.Rmd produces expected output", {
  data(wastewater_data, package = "DSIWasteWater")
  DF_data <- wastewater_data
  
  expect_identical(digest::digest(DF_data),"dfec63c14ed23c7f8829c074296043d6")
  
  workset4_data <- buildWorkSheet4(wastewater_data)
  
  workset4_data <- workset4_data%>% 
    filter(n >= 180)
  
  expect_identical(digest::digest(workset4_data),"9e0deb36d570d2ba0800de04043717bd")
  
  reg_estimates_data <- buildRegressionEstimateTable(workset4_data)
  
  expect_identical(digest::digest(reg_estimates_data),"e814c899d1dd4db02afe3179b3a7c465")
  
  DHSPlot <- createDHSMethod_Plot(reg_estimates_data, workset4_data)
  
  vdiffr::expect_doppelganger("Vignette1Plot", DHSPlot)
  
  reg_estimates_Reduced_data <- reg_estimates_data[
    reg_estimates_data$WWTP == "Madison MSD WWTF",
  ]
  
  workset4_Reduced_data <- workset4_data[
    workset4_data$WWTP == "Madison MSD WWTF",
  ]
  
  DHSPlot_Madison <- createDHSMethod_Plot(reg_estimates_Reduced_data, workset4_Reduced_data)
  
  vdiffr::expect_doppelganger("Vignette1Plot_Madison", DHSPlot_Madison)
})