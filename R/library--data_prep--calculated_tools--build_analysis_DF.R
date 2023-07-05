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
    mutate(N1 = ifelse(N1, 
                                      as.numeric(n1_lod)/2, N1),
           N2 = ifelse(N2, 
                                      as.numeric(n2_lod)/2, N2))%>%
    select(
      site,date,pop,  ## site data
      N1, N2,             ## N1, N2 measurement
      flow                                 ## sample covariates
    ) %>% 
    filter(!is.na(flow))%>% 
    mutate (geoMean = sqrt(N1*N2),
            sars_cov2_adj_load_log10 = log10(geoMean*flow/pop))%>% 
    group_by(site)%>% 
    mutate(n = n())%>% 
    arrange(date, .by_group = TRUE) %>% 
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
    arrange(site, date)%>%
    group_by(site)%>%
    #Create case data norm by the population
    mutate(FirstConfirmed.Per100K = (conf_case * 100000) / population_served,
    #get rolling sum of the last 7 days filling missing data with NAs
            pastwk.sum.casesperday.Per100K = 
                        rollsumr(conf_case, 7, fill=NA),
            pastwk.avg.casesperday.Per100K = pastwk.sum.casesperday.Per100K / 7)%>%
    ungroup()
  
  return(CaseProcess)
}
