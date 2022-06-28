test_that("Test vignettes vignettes_DHSTopLevelAnalysis_Outlier produces expected output", {
  data(wastewater_data, package = "DSIWasteWater")
  DF <- wastewater_data
  
  workset4_data <- buildWorkSheet4(wastewater_data)
  
  workset4_data <- workset4_data%>% 
    filter(n >= 180)
  
  workset4_Smooth_data <- workset4_data%>%
    group_by(WWTP)%>%
    group_split()%>%
    lapply(LoessSmoothMod)%>%
    bind_rows()
  
  expect_identical(digest::digest(workset4_Smooth_data),"a9da2f351670db1d8c82c17a84f65727")
  
  df_data <- computeJumps(workset4_Smooth_data)
  
  expect_identical(digest::digest(df_data),"5a9a04ce2af8bd909824cab204e84c60")
  
  ranked_data <- rankJumps(df_data)
  
  expect_identical(digest::digest(ranked_data),"65da6677d3d1a47989bef4f59b53bf12")
  
  ranked_quantile_data <- computeRankQuantiles(ranked_data)
  
  expect_identical(digest::digest(ranked_quantile_data),"2071d994fd0ed0f4d5da6b5b5035c333")
  
  classied_data <- flagOutliers(ranked_quantile_data, 9)
  
  expect_identical(digest::digest(classied_data),"3cc3719f662823d931adfa4de2abf6a9")
  
  created_data <- RemoveOutliers(classied_data)
  
  expect_identical(digest::digest(created_data),"95f751038ce32e89f3cf21323836db6a")
  
  reg_estimates_data <- buildRegressionEstimateTable(created_data,
                                                     RunOn = c("sars_cov2_adj_load_log10",
                                                               "sars_adj_log10_Filtered",
                                                               "Loess"))
  expect_identical(digest::digest(reg_estimates_data),"5515a6867d151eb13432798c6fa83cf5")
  
  

  DHSPlot <- createDHSMethod_Plot(reg_estimates_data, created_data, 
                       PointVal = c( "sars_cov2_adj_load_log10",
                                     "sars_adj_log10_Filtered"),
                       LineVal = "Loess")
  
  
  vdiffr::expect_doppelganger("Vignette2Plot", DHSPlot)
})