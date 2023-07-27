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
#' data("example_data", package = "DSIWastewater")
#' example_data$flag = 1
#' DF_date_vector(example_data, date, flag)
DF_date_vector <- function(DF, date_vec, flag_vecs){
  retDF <- DF%>%
    mutate(across(all_of({{flag_vecs}}), ~ifelse(.x == 1, {{date_vec}}, NA)))%>%
    mutate(across(all_of({{flag_vecs}}), ~as.Date(.x, origin = .Date(0))))
  return(retDF)
}

#' lookup
#'
#' @param DFCol date flag vector used to find min distance
#' @param base_date_vec date flag vector used to find baseline min distance
#'
#' @return difference between DFCol and the closest entry of base_date_vec
#' @keywords internal
#' 
#' @examples
#' data("example_data", package = "DSIWastewater")
#' DSIWastewater:::diffLookup(example_data$geoMean, example_data$n)
diffLookup <- function(DFCol, base_date_vec){
  sorted_base_vec <- sort(base_date_vec)
  sorted_base_Lookup <- stepfun(sorted_base_vec, 0:length(sorted_base_vec))
  indices <- pmin(pmax(1, sorted_base_Lookup(DFCol)), length(sorted_base_vec) - 1)
  mindistA <- DFCol - sorted_base_vec[indices]
  mindistB <- DFCol - sorted_base_vec[indices + 1]
  mindist <- pmin(abs(mindistA), abs(mindistB))
  mindist <- ifelse(mindist == abs(mindistA), mindistA, mindistB)
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
#' date_distance_calc(example_data, geoMean, n)
date_distance_calc <- function(DF, base_date_vec, vecNames){
  RetDF <- DF%>%
    group_by(.data$site)%>%
    mutate(across(all_of({{vecNames}}), 
                  ~diffLookup(.x, {{base_date_vec}})))
  return(RetDF)
}


#' remove distances above threshold
#'
#' @param thresh max distance not reduced to thresh from data
#' @param DF source dataframe
#' @param vecNames column names in DF to be modified
#'
#' @return DF with distances above threshold clamped to threshold
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' df <- date_distance_calc(example_data, geoMean, n)
#' date_distance_remove(df, geoMean, 21)
date_distance_clamp <- function(DF, vecNames, thresh){
  RetDF <- DF%>%
    mutate(across(all_of({{vecNames}}), 
                  ~ifelse(abs(.x) > thresh, thresh * sign(.x), .x)))
  return(RetDF)
}

#' remove distances above threshold
#'
#' @param thresh max distance not removed from data
#' @param DF source dataframe
#' @param vecNames column names in DF to be modified
#'
#' @return DF with distances above threshold removed
#' @export
#'
#' @examples
#' data("example_data", package = "DSIWastewater")
#' df <- date_distance_calc(example_data, geoMean, n)
#' date_distance_remove(df, geoMean, 21)
date_distance_remove <- function(DF, vecNames, thresh){
  RetDF <- DF%>%
    mutate(across(all_of({{vecNames}}), 
                  ~ifelse(abs(.x) > thresh, NA, .x)))
  return(RetDF)
}

