#' Convert wastewater_data data to workset4 shape
#' 
#' This takes the wastewater_data dataframe and rename variables, 
#' calculates sars_cov2_adj_load_log10 column, 
#' and filters rows where average_flow_rate is NA
#' 
#'
#' @param df data frame object from data/wastewater_data.rda 
#'
#' @return data frame
#' @export
#'
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' buildWasteAnalysisDF(WasteWater_data)
buildWasteAnalysisDF <- function(df){
  ## format data as DHS code expects
  
  ### Note: Replacement small values with LOD/2 (as per 5/20/2022 discussion w/DHS)
  df <- df %>% 
    mutate(N1 = ifelse(.data$N1, as.numeric(.data$n1_lod)/2, .data$N1),
           N2 = ifelse(.data$N2, as.numeric(.data$n2_lod)/2, .data$N2))%>%
    select(
      .data$site, .data$date, .data$pop,  ## site data
      .data$N1, .data$N2,             ## N1, N2 measurement
      .data$flow                                 ## sample covariates
    ) %>% 
    filter(!is.na(.data$flow))%>% 
    mutate (geoMean = sqrt(.data$N1 * .data$N2),
            sars_cov2_adj_load_log10 = log10(.data$geoMean * .data$flow / .data$pop))%>% 
    group_by(.data$site)%>% 
    mutate(n = n())%>% 
    arrange(.data$date, .by_group = TRUE) %>% 
    ungroup()
  return(df)
}

#' Prep case data into right format
#'
#' @param df case dataframe have columns: Date, population_served, FirstConfirmed
#'
#' @return DF with a 7 day rolling sum and a population weighted case column
#' @export
#'
#' @examples
#' data(Case_data, package = "DSIWastewater")
#' buildCaseAnalysisDF(Case_data)
buildCaseAnalysisDF <- function(df){
  CaseProcess <- df%>%
    #sort data to make sure the rolling sum func does not fail to sum correctly
    arrange(.data$site, .data$date)%>%
    group_by(.data$site)%>%
    #Create case data norm by the population
    mutate(FirstConfirmed.Per100K = (.data$conf_case * 100000) / .data$population_served,
    #get rolling sum of the last 7 days filling missing data with NAs
            pastwk.sum.casesperday.Per100K = rollsumr(.data$conf_case, 7, fill=NA),
            pastwk.avg.casesperday.Per100K = .data$pastwk.sum.casesperday.Per100K / 7)%>%
    ungroup()
  
  return(CaseProcess)
}
