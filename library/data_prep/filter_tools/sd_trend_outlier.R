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
#' library(dplyr)
#' data("Example_data", package = "DSIWastewater")
#' smoothing_df <- Example_data%>%
#'     select(site, date, N1, N2)%>%
#'     filter(N1 != 0, N2 != 0)%>%
#'     mutate(N1 = log(N1), N2 = log(N2), N12_avg = (N1 + N2) / 2)
#'     df_data <- loessSmoothMod(smoothing_df, "N12_avg", "N12_avg_loess", Filter = NULL)
#' head(df_data%>%
#'    group_by(site)%>%
#'    Flag_From_Trend( N12_avg, N12_avg_loess)%>%
#'    select(site, date, N12_avg, flagged_outlier))
Flag_From_Trend <- function(DF, base_data, trend_data, sd_degree = 2.5){
  #should check if error is constant over time
  #should check if error is constant over scale
  return_df <- DF%>%
    mutate(detrended_data = abs({{base_data}} - {{trend_data}}),
           flagged_outlier = .data$detrended_data > sd_degree * sd(.data$detrended_data))%>%
    select(-.data$detrended_data)
  return(return_df)
}
