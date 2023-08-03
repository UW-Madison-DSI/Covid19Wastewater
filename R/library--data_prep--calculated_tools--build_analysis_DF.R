#' Convert wastewater_data data to workset4 shape
#' 
#' This takes the wastewater_data dataframe and rename variables, 
#' calculates sars_cov2_adj_load_log10 column, 
#' and filters rows where average_flow_rate is NA
#' 
#' @param df data frame object from data/wastewater_data.rda 
#'
#' @return data frame
#' @export
#'
#' @examples
#' data(WasteWater_data, package = "Covid19Wastewater")
#' data(Pop_data, package = "Covid19Wastewater")
#' buildWasteAnalysisDF(dplyr::left_join(head(WasteWater_data), Pop_data))
buildWasteAnalysisDF <- function(df){
  # format data as DHS code expects
  # Note: Replacement small values with LOD/2 (as per 5/20/2022 discussion w/DHS)
  df <- df %>% 
    mutate(N1 = ifelse(.data$N1, as.numeric(.data$n1_lod)/2, .data$N1),
           N2 = ifelse(.data$N2, as.numeric(.data$n2_lod)/2, .data$N2))%>%
    select(
      .data$sample_id, .data$site, .data$date, .data$pop,  ## site data
      .data$N1, .data$N2,             ## N1, N2 measurement
      .data$flow                                 ## sample covariates
    ) %>% 
    filter(!is.na(.data$flow))%>% 
    mutate (geoMean = sqrt(.data$N1 * .data$N2),
            sars_cov2_adj_load_log10 = log10(1 + (.data$geoMean * .data$flow / .data$pop)))%>% 
    group_by(.data$site)%>% 
    mutate(n = n())%>% 
    arrange(.data$date, .by_group = TRUE) %>% 
    ungroup()
  return(df)
}

#' Prep case data into right format
#'
#' @param df case dataframe have columns: Date, pop, FirstConfirmed
#' @param site_column name of site column
#' @param date_column name of date column
#' @param case_column name of case column
#' @param pop_column name of pop column
#'
#' @return DF with a 7 day rolling sum and a population weighted case column
#' @export
#'
#' @examples
#' data(Case_data, package = "Covid19Wastewater")
#' data(Pop_data, package = "Covid19Wastewater")
#' buildCaseAnalysisDF(dplyr::left_join(head(Case_data), Pop_data))
buildCaseAnalysisDF <- function(df,                          
                                site_column = site,
                                date_column = date, 
                                case_column = conf_case,
                                pop_column = pop){
  site <- date <- conf_case <- pop <- NA #default column for function. Not evaluated as NA in dplyr context
  
  CaseProcess <- df%>%
    #sort data to make sure the rolling sum func does not fail to sum correctly
    arrange({{site_column}}, {{date_column}})%>%
    group_by({{site_column}})%>%
    #Create case data norm by the population
    mutate(FirstConfirmed.Per100K = ({{case_column}} * 100000) / {{pop_column}},
    #get rolling sum of the last 7 days filling missing data with NAs
            pastwk.sum.casesperday.Per100K = rollsumr({{case_column}}, 7, fill=NA),
            pastwk.avg.casesperday.Per100K = .data$pastwk.sum.casesperday.Per100K / 7)%>%
    ungroup()
  
  return(CaseProcess)
}