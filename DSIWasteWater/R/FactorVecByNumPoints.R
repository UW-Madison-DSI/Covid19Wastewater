#' Title
#'
#' @param DF 
#' @param FacVar 
#' @param FiltVar 
#'
#' @return
#' @export
#'
#' @examples
FactorVecByNumPoints <- function(DF,FacVar, FiltVar){
  FactorOrder <- (DF%>%
                    filter(!is.na(!!sym(FiltVar)))%>%
                    group_by(!!sym(FacVar))%>%
                    summarise(n=n())%>%
                    arrange(desc(n)))[[FacVar]]
  
  
  FacedDF <- DF%>%
    mutate(!!FacVar := factor(!!sym(FacVar),FactorOrder))
  
  return(FacedDF)
}