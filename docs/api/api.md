# Wastewater API Reference

In order to view more detail about each of these API methods, you can [view the API in the R Studio Help](../r-studio/viewing-api.md).

## Smoothing Functions

- ### LoessSmoothMod
  Adds a column of the smoothed values using Loess smoothing.

- ### ExpSmoothMod
  Adds a column of the smoothed values using exponential smoothing.

- ### sgolaySmoothMod
  Adds a column of the smoothed values using sgolayfilt.

## Flagging Functions

- ### flagOutliers
  Creates column with Boolean based on a threshold.

- ### CreateCaseFlag
  Creates case flags.

- ### CreateWasteFlags
  Creates waste flags.
  
- ### Count_Flag
  Creates counts of flag data.

## Data Processing Functions

- ### buildCaseAnalysisDF
  Preps case data into the right format.

- ### buildWasteAnalysisDF
  Converts wastewater_data data to workset4 shape.
  
- ### computeJumps
  Computes first difference jumps for N1 and N2.

- ### computeRankQuantiles
  Computes ranked quantiles.

- ### FactorVecByNumPoints
  Converts column to factor based on amount of entries.

- ### MakeQuantileColumns
  Adds many combo of rolling quantile columns to dataframe have info for each quant window combo.
  
- ### RemoveOutliers
  Adds column with NA values where the data was flagged.

- ### uniqueVal
  Finds all unique values in the column selected.

- ### WindowingQuantFunc
  Creates rolling quantile column based on date.

## Analysis Functions

- ### buildRegressionEstimateTable
  Runs DHS analysis at a top level.

- ### ClassifyCaseRegression
  Creates case flags based on regression slope.

- ### ClassifyQuantileFlagRegression
  Classifies and flags regression with rolling quantile info.

- ### ClassifyRegressionAnalysis
  Classifies regression analysis.

- ### NGuess
  Guesses number of points per polynomial for sgolaySmoothMod.

- ### ParameterGuess
  Gets a fitting parameter for the model.

- ### rankJumps
  Ranks jumps.

- ### runRegressionAnalysis
  Runs regression analysis.
  
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
  Takes two list of plots and combines them into one 3 col long plot.
  
## Data Sets
  
- ### Case_data
  Case data set from the DHS on 4/21/2022.

- ### InterceptorCase_data
  Madison interceptor Case data set.
  
- ### WasteWater_data
  Wastewater data set cleaned from the surveillance branch.
  
- ### example_data
  Toy example of worksheet 4 to be used in package examples.
  
## Packages

- ### DSIWastewater
  A package for running Covid wastewater concentration analysis.