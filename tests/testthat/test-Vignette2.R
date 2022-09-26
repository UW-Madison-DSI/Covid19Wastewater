test_that("Test vignettes vignettes_DHSTopLevelAnalysis_Outlier produces expected output", {
  data(WasteWater_data, package = "DSIWastewater")
  
  baseWaste_data <- buildWasteAnalysisDF(WasteWater_data)
  
  baseWaste_data <- baseWaste_data%>% 
    filter(n >= 180)
  
  workset4_Smooth_data <- baseWaste_data%>%
    group_by(site)%>%
    group_split()%>%
    lapply(loessSmoothMod)%>%
    bind_rows()
  
  expect_identical(digest::digest(workset4_Smooth_data),"ed472d973d2ce9f75f03073446b0d00c")
  
  df_data <- computeJumps(workset4_Smooth_data)
  
  expect_identical(digest::digest(df_data),"7a454aad75d847afe03599808524f427")
  
  ranked_data <- rankJumps(df_data)
  
  expect_identical(digest::digest(ranked_data),"64f6f574f2a623b1bbc28152057b84a6")
  
  ranked_quantile_data <- computeRankQuantiles(ranked_data)
  
  expect_identical(digest::digest(ranked_quantile_data),"caa965e88af6d2169f9b5d335513ef1e")
  
  classied_data <- flagOutliers(ranked_quantile_data, 9)
  
  expect_identical(digest::digest(classied_data),"ccaaaf830d3264ac17be2c601431b1a2")
  
  created_data <- removeOutliers(classied_data)
  
  expect_identical(digest::digest(created_data),"24ae6cb00df5fb4feab0bd0fae54237b")
  
  reg_estimates_data <- buildRegressionEstimateTable(created_data,
                                                     RunOn = c("sars_cov2_adj_load_log10",
                                                               "sars_adj_log10_Filtered",
                                                               "Loess"))
  expect_identical(digest::digest(reg_estimates_data),"2212ab89ece65876de92fb3b0fcaf661")
  
  

  DHSPlot <- createRegressionAnalysis_Plot(reg_estimates_data, created_data, 
                       PointVal = c( "sars_cov2_adj_load_log10",
                                     "sars_adj_log10_Filtered"),
                       LineVal = "Loess")
  
  
  vdiffr::expect_doppelganger("Vignette2Plot", DHSPlot)
})
