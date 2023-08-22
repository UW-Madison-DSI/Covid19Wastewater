#' Wastewater data set
#'
#' Data from the DHS of wastewater prevalence in Wisconsin cites
#'
#' @docType data
#'
#' @usage data("WasteWater_data")
#'
#' @format Rows: 11,084 Columns: 62
#'
#' @keywords datasets
"WasteWater_data"


#' Example data
#' 
#' Toy example of full data contains 3 sites and roughly 500 entries 
#' (waste and case merged together)
#' 
#' @docType data
#'
#' @usage data("Example_data")
#'
#' @format Rows: 500 Columns: 69
#'
#' @keywords datasets
"Example_data"


#' Case data
#'
#' Data from the DHS of positive covid tests in Wisconsin cites
#'
#' @docType data
#'
#' @usage data("Case_data")
#'
#' @format Rows: 33103  Columns: 4
#'
#' @keywords datasets
"Case_data"


#' Madison interceptor case data
#'
#' Data from the DHS of positive covid tests for Madison interceptors
#'
#' @docType data
#'
#' @usage data("InterceptorCase_data")
#'
#' @format Rows: 3288   Columns: 5
#'
#' @keywords datasets
"InterceptorCase_data"


#' High frequency Waste data
#'
#' Data from the DHS of  HFG covid wastewater concentration
#'
#' @docType data
#'
#' @usage data("HFGWaste_data")
#'
#' @format Rows: 3078   Columns: 18
#'
#' @keywords datasets
"HFGWaste_data"


#' High frequency case data
#'
#' Data from the DHS of positive covid tests for HFG data
#'
#' @docType data
#'
#' @usage data("HFGCase_data")
#'
#' @format Rows: 788   Columns: 6
#'
#' @keywords datasets
"HFGCase_data"


#' Sewer shed population data
#'
#' dataframe containing population information about each sewer shed in Wisconsin
#'
#' @docType data
#'
#' @usage data("Pop_data")
#'
#' @format Rows: 89   Columns: 2
#'
#' @keywords datasets
"Pop_data"


#' Covariants data
#'
#' dataframe containing info about the proportion of each COVID-19 variant
#' every 2 weeks. This data is from 
#' [https://github.com/hodcroftlab/covariants/tree/master](https://github.com/hodcroftlab/covariants/tree/master)
#'
#' @docType data
#'
#' @usage data("Covariants_data")
#'
#' @format Rows: 69   Columns: 33
#'
#' @keywords datasets
"Covariants_data"


#' Auxiliary data
#'
#' auxiliary information that has been split from the WasteWater_data. Can be 
#' re-merged using merge(WasteWater_data, Aux_info_data, by="sample_id")
#'
#' @docType data
#'
#' @usage data("Aux_info_data")
#'
#' @format Rows: 999   Columns: 17
#'
#' @keywords datasets
"Aux_info_data"