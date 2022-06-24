test_that("Test vignettes DHSTopLevelAnalysis_Base.Rmd produces expected output", {
  data(wastewater_data, package = "DSIWasteWater")
  DF <- wastewater_data
  
  expect_identical(digest::digest(DF),"dfec63c14ed23c7f8829c074296043d6")
  
  workset4 <- buildWorkSheet4(wastewater_data)
  
  workset4 <- workset4%>% 
    filter(n >= 150)
  
  expect_identical(digest::digest(workset4),"3c7d779a66b8c81a3d512b54e1c15279")
  
  reg_estimates <- buildRegressionEstimateTable(workset4)
  
  expect_identical(digest::digest(reg_estimates),"fee9f7483109c0fb2c7bade3e3496463")
  
  DHSPlot <- createDHSMethod_Plot(reg_estimates, workset4)
  
  vdiffr::expect_doppelganger("Vignette1Plot", DHSPlot)
})