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
     mutate(n1_sars_cov2_lod = gsub(" ", "",tolower(n1_sars_cov2_lod)) == "yes",
         n2_sars_cov2_lod = gsub(" ", "",tolower(n2_sars_cov2_lod)) == "yes",
         n1_sars_cov2_conc = ifelse(n1_sars_cov2_lod, 
                                    as.numeric(n1_lod)/2, n1_sars_cov2_conc),
         n2_sars_cov2_conc = ifelse(n2_sars_cov2_lod, 
                                    as.numeric(n2_lod)/2, n2_sars_cov2_conc),
         population_served = as.numeric(gsub(",", "",population_served)))%>%
    select(
      wwtp_name,sample_collect_date,population_served,  ## site data
      n1_sars_cov2_conc, n2_sars_cov2_conc,             ## N1, N2 measurement
      average_flow_rate                                 ## sample covariates
    ) %>% 
    rename(site = wwtp_name, date = sample_collect_date) %>% 
    mutate(date = as.Date(date,format="%m/%d/%Y"))
  
  ## dependent regression variable: log of normalized average SARS-COV-2 level
  workset4 <- df %>% 
    filter(average_flow_rate != "NA") %>% 
    mutate (geoMean = sqrt(n1_sars_cov2_conc*n2_sars_cov2_conc)) %>% 
    mutate(sars_cov2_adj_load_log10 = log10(
      geoMean*average_flow_rate/population_served)
    )

  ## add number of point info and sort
  workset4 <- workset4 %>% 
    group_by(site) %>% 
    mutate(n = n()) %>% 
    arrange(date, .by_group = TRUE) %>% 
    ungroup()
  return(workset4)
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
    rename(site = Site)%>%
    #sort data to make sure the rolling sum func does not fail to sum correctly
    arrange(site, date)%>%
    group_by(site)%>%
    #Create case data norm by the population
    mutate(FirstConfirmed.Per100K = (FirstConfirmed * 100000) / population_served,
    #get rolling sum of the last 6 days filling missing data with NAs
            pastwk.sum.casesperday.Per100K = 
                        rollsumr(FirstConfirmed.Per100K, 7, fill=NA),
            pastwk.avg.casesperday.Per100K = pastwk.sum.casesperday.Per100K / 7)
  
  return(CaseProcess)
}
