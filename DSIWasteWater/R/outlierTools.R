#' computeJumps
#'
#' @param n/a
#' 
#' @return dataframe with 4 columns appended: delta(n1), delta(n2) from left and right
#' @export
#'
#' @examples
computeJumps <- function(df) {
  df <- df %>% 
    group_by(WWTP) %>% 
    mutate(
      n1.before = lag(n1_sars_cov2_conc, order_by = WWTP),
      n1.after  = lead(n1_sars_cov2_conc, order_by = WWTP),
      n2.before = lag(n2_sars_cov2_conc, order_by = WWTP),
      n2.after  = lead(n2_sars_cov2_conc, order_by = WWTP)
    ) %>% 
    mutate(
      n1.jumpFromLeft  = n1_sars_cov2_conc - n1.before,
      n1.jumpFromRight = n1_sars_cov2_conc - n1.after,
      n2.jumpFromLeft  = n2_sars_cov2_conc - n2.before,
      n2.jumpFromRight = n2_sars_cov2_conc - n2.after
    ) %>% 
    select(-c(n1.before,n1.after,n2.before,n2.after))
  return(df)
}

#' rankJumps
#'
#' @param n/a
#' 
#' @return dataframe with 4 columns appended: ranks of each of the 4 jumps;
#' @export
#'
#' @examples
rankJumps <- function(df) {
  df <- df %>% 
    group_by(WWTP)   %>% 
    mutate(rank.n1.jumpFromLeft = rank(-n1.jumpFromLeft)) %>% 
    mutate(rank.n1.jumpFromRight = rank(-n1.jumpFromRight)) %>% 
    
    mutate(rank.n2.jumpFromLeft = rank(-n2.jumpFromLeft)) %>% 
    mutate(rank.n2.jumpFromRight = rank(-n2.jumpFromRight)) %>% 

    ## sort by first jump ranks just to be definitive
    arrange(WWTP,rank.n1.jumpFromLeft) 
  return(df)
}

#' computeRankQuantiles
#'
#' @param n/a
#' 
#' @return 
#' @export
#'
#' @examples
computeRankQuantiles <- function(df) {
  df <- df %>% 
    group_by(WWTP) %>% 
    mutate(numValues = n()) %>% 
    mutate(
      n1.jumpFromLeft.quantile  = rank.n1.jumpFromLeft/numValues,
      n1.jumpFromRight.quantile = rank.n1.jumpFromRight/numValues,

      n2.jumpFromLeft.quantile  = rank.n2.jumpFromLeft/numValues,
      n2.jumpFromRight.quantile = rank.n2.jumpFromRight/numValues
    ) %>%
    select(-numValues) %>%
    
    ## sort by first jump ranks just to be definitive
    arrange(WWTP,n1.jumpFromLeft.quantile)   
  
}