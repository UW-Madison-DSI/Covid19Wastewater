#' Create counts of flag data
#' 
#' Takes a data frame with grouping columns and numeric flag DF where 1 means 
#' a flag
#'
#' @param DF data frame with grouping columns and numeric flag columns
#' @param group vector specifying what columns should be used for grouping
#'
#' @return DF with the number of flags of each type with in groups
#' @export
Count_Flag <- function(DF, 
                       group = c("Site", "window", "quant")){
  
  Count_DF <- DF%>%
    #grouping by the proup parameter
    group_by(across(group))%>%
    #return sum of each column that is of type numeric
    summarise(across(where(is.numeric), sum), .groups = "keep")
  
  return(Count_DF)
}