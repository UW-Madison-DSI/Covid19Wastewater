test_that("Test vignettes DHSTopLevelAnalysis_Base.Rmd produces expected output", {
  data(DHSWaste_data, package = "DSIWastewater")
  DF_data <- DHSWaste_data
  
  expect_identical(digest::digest(DF_data),"24eeac5cf5e258881882809d45daa4d7")
  
  workset4_data <- buildWorkSheet4(DHSWaste_data)
  
  workset4_data <- workset4_data%>% 
    filter(n >= 180)
  
  expect_identical(digest::digest(workset4_data),"5d530c77c4ac8b80e47d1f2857bdf0dc")
  
  reg_estimates_data <- buildRegressionEstimateTable(workset4_data)
  
  expect_identical(digest::digest(reg_estimates_data),"bf1480a0b06404c0c79301e1f07dcc09")
  
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