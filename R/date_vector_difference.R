#' DF_date_vector
#'
#' @param DF DF containing the other vectors as columns
#' @param date_vec column vec name with date dataType
#' @param flag_vecs a vector of column vec names each with binary flagging info
#'
#' @return an equivilent DF where the 1 in the flag info is replaced with the date
#' @export
#'
#' @examples
#' @example 
#' data("example_data", package = "DSIWastewater")
#' example_data$flag = 1
#' DF_date_vector(example_data, "date", "flag")
DF_date_vector <- function(DF, date_vec, flag_vecs){
  retDF <- DF%>%
    mutate(across(all_of(flag_vecs), ~ifelse(.x==1, !!sym(date_vec), NA)))%>%
    mutate(across(all_of(flag_vecs), ~as.Date(.x, origin = .Date(0))))
  return(retDF)
}

#' lookup
#'
#' @param DFCol date flag vector used to find min distance
#' @param base_date_vec date flag vector used to find baseline min distance
#'
#' @return diffrence between DFCol and the closest entry of base_date_vec
#' @keywords internal
#' 
#' @example
#' data("example_data", package = "DSIWastewater")
#' diffLookup(example_data$geoMean, example_data$n)
diffLookup <- function(DFCol, base_date_vec, edge = NA){
  sorted_base_vec <- sort(base_date_vec)
  sorted_base_Lookup <- stepfun(sorted_base_vec, 0:length(sorted_base_vec))
  indices <- pmin(pmax(1, sorted_base_Lookup(DFCol)), length(sorted_base_vec) - 1)
  mindist <- pmin(abs(DFCol - sorted_base_vec[indices]), 
                  abs(DFCol - sorted_base_vec[indices + 1]))
  if(!is.na(edge)){
    mindist <- ifelse(mindist>edge, edge, mindist)
  }
  return(mindist)
}

#' date_distance_calc
#'
#' @param DF DF to extract vector from
#' @param base_date_vec date flag vector to pull each distance from
#' @param vecNames a vector of column vec names each with binary flagging info
#'
#' @return DF containing the distance of each term to the base vector
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' date_distance_calc(example_data, "geoMean", "n")
date_distance_calc <- function(DF, base_date_vec, vecNames, edge = NA){
  RetDF <- DF%>%
    mutate(across(all_of(vecNames), 
                  ~as.numeric(diffLookup(.x, DF[[base_date_vec]], edge = edge))))
  return(RetDF)
}