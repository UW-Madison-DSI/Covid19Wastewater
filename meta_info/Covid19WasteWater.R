#' Covid19Wastewater: A package for running Covid19 wastewater concentration analysis
#'
#' This is an R package of utilities to perform wastewater data analysis for pathogenic surveillance and monitoring. 
#' 
#' This project is a collaboration between the University of Wiscosin's [Data Science Institute (DSI)](https://datascience.wisc.edu), the [Wisconsin Department of Health Services (DHS)]()https://www.dhs.wisconsin.gov/covid-19/wastewater.htm), and the [Wisconsin State Lab of Hygiene (SLH)](https://www.slh.wisc.edu/environmental/covid-19-wastewater/).
#' 
#' If you need help getting started view our [getting started guide](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/getting-started/Starting_Guide.md)
#' 
#' 
#' @section Built in data:
#' View our data vignettes to learn more about our built in data.
#' To view what each column name means view our [Data_Description](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/data/data_columns_discription.md)
#' 
#' This package includes traditional (case-based) and wastewater-based data about Wisconsin communities.
#' 
#' * Longitudinal Data: This dataset includes longitudinal wastewater data collected 1-6 times per week from 2019 to 2023. It is accompanied by case data. The dataset files are:  [WasteWater_data.RData](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/longitudinal_data_waste.pdf), [Case_data.RData](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/longitudinal_data_case.pdf)
#' 
#' * High-Frequency Data: This dataset contains high-frequency testing data collected over a 7-day testing period. It includes 9 samples reported per day and is accompanied by corresponding case data. The dataset files are:  [HFG_data.RData](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/HFG_data_waste.pdf), [HFGCase_data.RData](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/HFG_data_case.pdf)
#' 
#' * Extra Information: The package also provides additional information in the form of dataframes. The extra info datasets are as follows:  [Pop_data.RData](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/population_data.pdf), [Covariants_data.RData](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/variant_data.pdf)
#' 
#' 
#' @section Data Prep:
#' Learn how to prep our data here for analysis
#' 
#' The following are the data preparation tools used in this package:
#' 
#' * Outliers: This vignette illustrates techniques for flagging outliers: [outliers.Rmd](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/outliers.pdf)
#' 
#' * Smoothing: This vignette is an illustration of the smoothing techniques that are available in this package: [smoothing.Rmd](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/smoothing.pdf)
#' 
#' * Calculated Info: This vignette computed additiional auxilliary information (geometric means, population normalizing, case averaging): [calculated_info.Rmd](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/calculated_info.pdf)
#' 
#' 
#' @section Analysis tools:
#' Learn how to analize using our methods or create your own.
#' 
#' The following are the analysis tools used in this package:
#'   
#' * DHS Methods: This code comes from our collaborators at the DHS and is an example of of their flagging method that is used on their [dashboard](https://www.dhs.wisconsin.gov/covid-19/wastewater.htm)
#' * Time Series Offset: This vignette demonstrates how to use the offset analysis tools to find the temporal offset in days between wastewater and case measurements: [time_series_offset.Rmd](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/time_series_offset.pdf)
#' * Linear Forest: This package contains a Linear Forest modeling tool for finding an optimal breakdown of linear components. This is most useful for looking for cofactors to the N1 vs case data: [linear_forest.Rmd](https://github.com/UW-Madison-DSI/Covid19Wastewater/blob/main/docs/vignettes/linear_forest.pdf)
#'
#'
#'View more in our github repo: [https://github.com/UW-Madison-DSI/Covid19Wastewater](https://github.com/UW-Madison-DSI/Covid19Wastewater)
#'
#' @docType package
#' @name Covid19Wastewater
NULL