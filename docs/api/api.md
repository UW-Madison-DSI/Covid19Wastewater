# Wastewater API Reference

## Plotting Functions

- ### Abstract_PlotAdd
  Adds a ggplot component.

- ### createConfMatrix_Plot
  Creates a conf matrix plot.

- ### createFlagGraph_plot
  Creates graphics showing flags as vertical lines.

- ### CreateHeatMaps_Plot
  Creates a heat maps plot.

- ### createMethodCompareBar_Plot
  Creates a comparative set of regression analysis plots.

- ### createRegressionAnalysis_Plot
  Creates a representative plot of the DHS analysis.

- ### createWasteGraph_Plot
  Creates a wastewater graphic.

- ### orderAndZipListsOfPlots_Plot
  Take two list of plots and combine them into one 3 col long plot

## Data Processing Functions

- ### buildCaseAnalysisDF
  Prep case data into right format

- ### buildWasteAnalysisDF
  Convert wastewater_data data to workset4 shape
  
- ### computeJumps
  compute first difference Jumps for N1 and N2

- ### computeRankQuantiles
  computeRankQuantiles

- ### Count_Flag
  Create counts of flag data

- ### CreateCaseFlag
  Create Case flags

- ### CreateWasteFlags
  Create waste flags
  
- ### ExpSmoothMod
  ExpSmoothMod Add a column of the smoothed values using exponential smoothing

- ### FactorVecByNumPoints
  convert column to factor based on amount of entries

- ### FactorVecByVec
  Title

- ### flagOutliers
  Create column with Boolean based on a threshold

- ### LoessSmoothMod
  LoessSmoothMod Add a column of the smoothed values using Loess

- ### MakeQuantileColumns
  Add many combo of rolling quantile columns to dataframe have info for each quant window combo
  
- ### RemoveOutliers
  Add column with NA values where the data was flagged

- ### uniqueVal
  Find all unique values in the column selected

- ### sgolaySmoothMod
  sgolaySmoothMod Add a column of the smoothed values using sgolayfilt

- ### WindowingQuantFunc
  create rolling quantile column based on date

## Analysis Functions

- ### buildRegressionEstimateTable
  Run DHS analysis at a top level

- ### ClassifyCaseRegression
  Create Case Flags based on regression slope

- ### ClassifyQuantileFlagRegression
  Classify FlagRegression with rolling Quantile info

- ### ClassifyRegressionAnalysis
  ClassifyRegressionAnalysis

- ### NGuess
  NGuess for sgolaySmoothMod number of points per polynomial

- ### ParameterGuess
  Get a fitting parameter for the model

- ### rankJumps
  rankJumps

- ### regressionInnerLoop
  regressionInnerLoop

- ### runRegressionAnalysis
  runRegressionAnalysis
  
## Data Sets
  
- ### Case_data
  Case data set from the DHS on 4/21/2022

- ### InterceptorCase_data
  Madison interceptor Case data set
  
- ### WasteWater_data
  Wastewater data set cleaned from the surveillance branch
  
- ### example_data
  Toy example of worksheet 4 to be used in package examples
  
## Packages

- ### DSIWastewater
  DSIWastewater: A package for running Covid wastewater concentration analysis
