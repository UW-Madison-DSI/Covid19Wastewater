#' compute first difference Jumps for N1 and N2
#'
#' @param df DataFrame. needs Column n1_sars_cov2_conc, n2_sars_cov2_conc, site
#' @param N1_column N1 metric used in finding difference
#' @param N2_column N2 metric used in finding difference
#' @param site_column Grouping that makes each group a time seriecomputeJumpss
#'
#' @return dataframe with 4 columns appended: delta(n1), delta(n2) from left and right
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' Example_data$site = "Madison"
#' computeJumps(Example_data)
computeJumps <- function(df, 
                         N1_column = N1,
                         N2_column = N2, 
                         site_column = site) {
  N1 <- N2 <- site <- NA #default column for jump. Not evaluated as NA in dplyr context
  df <- df %>% 
    #group_by({{site_column}})%>% 
    mutate(
      n1.before = lag({{N1_column}}, order_by = {{site_column}}),
      n1.after  = lead({{N1_column}}, order_by = {{site_column}}),
      n2.before = lag({{N2_column}}, order_by = {{site_column}}),
      n2.after  = lead({{N2_column}}, order_by = {{site_column}})
    ) %>% 
    mutate(
      n1.jumpFromLeft  = !{{N1_column}} - .data$n1.before,
      n1.jumpFromRight = {{N1_column}} - .data$n1.after,
      n2.jumpFromLeft  = {{N2_column}} - .data$n2.before,
      n2.jumpFromRight = {{N2_column}} - .data$n2.after
    ) %>% 
    select(-c(.data$n1.before, .data$n1.after, .data$n2.before, .data$n2.after))
  return(df)
}

#' rankJumps
#' 
#' Convert jumps from last step into a ordering
#'
#' @param df DataFrame. needs Column n1.jumpFromLeft, n1.jumpFromRight, 
#'           n2.jumpFromLeft, n2.jumpFromRight, site
#'           
#' First 4 gen from computeJumps
#' 
#' @return dataframe with 4 columns appended: ranks of each of the 4 jumps;
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' Example_data$site = "Madison"
#' df_data <- computeJumps(Example_data)
#' rankJumps(df_data)
rankJumps <- function(df) {
  df <- df %>% 
    group_by(.data$site)   %>% 
    mutate(rank.n1.jumpFromLeft = rank(-.data$n1.jumpFromLeft),
      rank.n1.jumpFromRight = rank(-.data$n1.jumpFromRight),
      rank.n2.jumpFromLeft = rank(-.data$n2.jumpFromLeft), 
      rank.n2.jumpFromRight = rank(-.data$n2.jumpFromRight),
      MessureRank = pmin(.data$rank.n1.jumpFromLeft, .data$rank.n1.jumpFromRight,
                         .data$rank.n2.jumpFromLeft, .data$rank.n2.jumpFromRight)
      ) %>% 
    ## sort by first jump ranks just to be definitive
    arrange(.data$site, .data$rank.n1.jumpFromLeft) 
  return(df)
}

#' computeRankQuantiles
#' 
#' Convert jumps from last step into a ordering quintile 
#'
#' @param df dataframe. needs Column n1.jumpFromLeft, n1.jumpFromRight, 
#'           n2.jumpFromLeft, n2.jumpFromRight, site
#'           
#' First 4 gen from computeJumps
#' 
#' @return dataframe with 4 columns appended: ranks of each of the 4 jumps;
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' Example_data$site = "Madison"
#' df_data <- computeJumps(Example_data)
#' ranked_data <- rankJumps(df_data)
#' computeRankQuantiles(ranked_data)
computeRankQuantiles <- function(df) {
  df <- df %>% 
    group_by(.data$site) %>% 
    mutate(numValues = n()) %>% 
    mutate(
      n1.jumpFromLeft.quantile  = .data$rank.n1.jumpFromLeft / .data$numValues,
      n1.jumpFromRight.quantile = .data$rank.n1.jumpFromRight / .data$numValues,

      n2.jumpFromLeft.quantile  = .data$rank.n2.jumpFromLeft / .data$numValues,
      n2.jumpFromRight.quantile = .data$rank.n2.jumpFromRight / .data$numValues,
      MessureRank.quantile = pmin(.data$n1.jumpFromLeft.quantile, 
                                  .data$n1.jumpFromRight.quantile, 
                                  .data$n2.jumpFromLeft.quantile, 
                                  .data$n2.jumpFromRight.quantile)
    ) %>%
    select(-.data$numValues) %>%
    
    ## sort by first jump ranks just to be definitive
    arrange(.data$site, .data$n1.jumpFromLeft.quantile)   
}

#' Create column with Boolean based on a threshold
#'
#' @param DF Dataframe containing Column column ranked_quantile_data 
#' @param threshold a numeric used to flag if its an outlier
#' @param col column being flagged based on threshold 
#' @param FlaggedOutlier name of flag column
#'
#' @return DF Dataframe with the extra column of if its flagged an outlier
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' Example_data$site = "Madison"
#' df_data <- computeJumps(Example_data)
#' ranked_data <- rankJumps(df_data)
#' ranked_quantile_data  <- computeRankQuantiles(ranked_data)
#' flagOutliers(ranked_quantile_data, 9, MessureRank, FlaggedOutlier)
flagOutliers <- function(DF, threshold, col, FlaggedOutlier = FlaggedOutlier){
  RetDF <- DF%>%
    mutate({{FlaggedOutlier}} := {{col}} < threshold)
  return(RetDF)
}

#' Add column with NA values where the data was flagged
#'
#' @param DF DF containing the columns Measure and Filtcol
#' @param Messure The original measurement we want to keep inliers for
#' @param Filtcol the column containing the Boolean info needed to remove outliers
#' @param outputColName the name for the clean column
#'
#' @return DF with new column without the flagged values
#' @export
#'
#' @examples
#' data(Example_data, package = "Covid19Wastewater")
#' Example_data$site = "Madison"
#' df_data <- computeJumps(Example_data)
#' ranked_data <- rankJumps(df_data)
#' ranked_quantile_data <- computeRankQuantiles(ranked_data)
#' classied_data <- flagOutliers(ranked_quantile_data, 9)
#' removeOutliers(classied_data, sars_cov2_adj_load_log10, FlaggedOutlier, sars_adj_log10_Filtered)
removeOutliers <- function(DF, Messure, Filtcol, outputColName){
  RetDF <- DF%>%
    mutate({{outputColName}} := ifelse({{Filtcol}}, NA, {{Messure}}))
  return(RetDF)
}