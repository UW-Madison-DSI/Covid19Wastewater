#' Flag values as outliers based on error from estimated trend
#' This function can be done within group if the data fed into it was grouped
#'
#' @param DF Dataframe containing selected columns
#' @param base_data Name of column containing Raw data
#' @param trend_data Name of generated trend data
#' @param sd_degree Threshold to flag deviant values as an outlier
#'
#' @return DF with a new column 'flagged_outlier' that contains if the column is an outlier
#' @export
#'
#' @examples
Flag_From_Trend <- function(DF, base_data, trend_data, sd_degree = 2.5){
  #should check if error is constant over time
  #should check if error is constant over scale
  return_df <- DF%>%
    mutate(detrended_data = abs({{base_data}} - {{trend_data}}),
           flagged_outlier = detrended_data > sd_degree * sd(detrended_data))%>%
    select(-detrended_data)
  return(return_df)
}