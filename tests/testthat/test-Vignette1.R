test_that("Test vignettes DHSTopLevelAnalysis_Base.Rmd produces expected output", {
  data(WasteWater_data, package = "DSIWastewater")
  DF_data <- WasteWater_data
  
  expect_identical(digest::digest(DF_data),"24eeac5cf5e258881882809d45daa4d7")
  
  baseWaste_data <- buildWasteAnalysisDF(WasteWater_data)
  
  baseWaste_data <- baseWaste_data%>% 
    filter(n >= 180)
  
  expect_identical(digest::digest(baseWaste_data),"42379b388a36102e2cca77347467da4d")
  
  reg_estimates_data <- buildRegressionEstimateTable(baseWaste_data)
  
  expect_identical(digest::digest(reg_estimates_data),"aa9cf463ffbdad98c7e72a0172528d52")
  
  DHSPlot <- createRegressionAnalysis_Plot(reg_estimates_data, baseWaste_data)
  
  vdiffr::expect_doppelganger("Vignette1Plot", DHSPlot)
  
  reg_estimates_Reduced_data <- reg_estimates_data[
    reg_estimates_data$site == "Madison MSD WWTF",
  ]
  
  workset4_Reduced_data <- baseWaste_data[
    baseWaste_data$site == "Madison MSD WWTF",
  ]
  
  DHSPlot_Madison <- createRegressionAnalysis_Plot(reg_estimates_Reduced_data, workset4_Reduced_data)
  
  vdiffr::expect_doppelganger("Vignette1Plot_Madison", DHSPlot_Madison)
})
