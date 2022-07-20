test_that("Test vignettes vignettes_DHSTopLevelAnalysis_Outlier produces expected output", {
  data(DHSWaste_data, package = "DSIWastewater")
  DF <- DHSWaste_data
  
  workset4_data <- buildWorkSheet4(DHSWaste_data)
  
  workset4_data <- workset4_data%>% 
    filter(n >= 180)
  
  workset4_Smooth_data <- workset4_data%>%
    group_by(WWTP)%>%
    group_split()%>%
    lapply(LoessSmoothMod)%>%
    bind_rows()
  
  expect_identical(digest::digest(workset4_Smooth_data),"cffbd30f8cc27d81f4f79f29647770ef")
  
  df_data <- computeJumps(workset4_Smooth_data)
  
  expect_identical(digest::digest(df_data),"b754feb98d04336ef52f0f89719b9f26")
  
  ranked_data <- rankJumps(df_data)
  
  expect_identical(digest::digest(ranked_data),"e3b23bef3cafe1cec5beb53ccb656c90")
  
  ranked_quantile_data <- computeRankQuantiles(ranked_data)
  
  expect_identical(digest::digest(ranked_quantile_data),"ef916fae88929ecad09159e3a8b7f768")
  
  classied_data <- flagOutliers(ranked_quantile_data, 9)
  
  expect_identical(digest::digest(classied_data),"3b7616b4ed1ee0a6e812edbe1cbe732c")
  
  created_data <- RemoveOutliers(classied_data)
  
  expect_identical(digest::digest(created_data),"ab4dc565f5feb7803e491693c8bd0ad6")
  
  reg_estimates_data <- buildRegressionEstimateTable(created_data,
                                                     RunOn = c("sars_cov2_adj_load_log10",
                                                               "sars_adj_log10_Filtered",
                                                               "Loess"))
  expect_identical(digest::digest(reg_estimates_data),"6983cf216761b946aa3671659423b881")
  
  

  DHSPlot <- createDHSMethod_Plot(reg_estimates_data, created_data, 
                       PointVal = c( "sars_cov2_adj_load_log10",
                                     "sars_adj_log10_Filtered"),
                       LineVal = "Loess")
  
  
  vdiffr::expect_doppelganger("Vignette2Plot", DHSPlot)
})