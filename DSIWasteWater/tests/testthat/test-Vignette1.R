test_that("Test vignettes DHSTopLevelAnalysis_Base.Rmd produces expected output", {
  DF <- data(Data_wastewater, package = "DSIWasteWater")
  
  expect_identical(digest::digest(DF),"10d1158b3becd28689f932ce5018f013")
  
  workset4 <- BuildWorkSheet4(Data_wastewater)
  
  workset4 <- workset4%>% 
    filter(n >= 150)
  
  expect_identical(digest::digest(workset4),"3c7d779a66b8c81a3d512b54e1c15279")
  
  reg_estimates <- BuildRegressionEstimateTable(workset4)
  
  expect_identical(digest::digest(reg_estimates),"e0922bb0966e1fa7265d77333e4ca69d")
  
  DHSPlot <- DHSTopLevelPlots(reg_estimates, workset4)
  
  vdiffr::expect_doppelganger("Vignette1Plot", DHSPlot)
})