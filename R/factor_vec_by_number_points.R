#' convert column to factor based on amount of entries
#'
#' @param DF The data frame being manipulated
#' @param FacVar The column being converted to a factor
#' @param FiltVar A column to filter before counting entrys.
#' Defaults to FacVar
#'
#' @return DF with the FacVar column a factor
FactorVecByNumPoints <- function(DF,FacVar, FiltVar = NA){
  if(is.na(FiltVar)){
    FiltVar <- FacVar
  }
  FactorOrder <- (DF%>%
                    filter(!is.na(!!sym(FiltVar)))%>%
                    group_by(!!sym(FacVar))%>%
                    summarise(n=n())%>%
                    arrange(desc(n)))[[FacVar]]
  
  
  FacedDF <- DF%>%
    mutate(!!FacVar := factor(!!sym(FacVar),FactorOrder))
  
  return(FacedDF)
}

#' Title
#'
#' @param FacVar 
#' @param FactorDF 
#' @param OrderDF 
#' @param NumVar 
#'
#' @return
#' @export
#'
#' @examples
FactorVecByVec <- function(FactorDF, OrderDF, FacVar, NumVar){

  FactorOrder <- (OrderDF%>%
                    filter(!is.na(!!sym(FacVar)), !is.na(!!sym(NumVar)))%>%
                    group_by(!!sym(FacVar))%>%
                    summarise(level = mean(!!sym(NumVar) ,na.rm = TRUE))%>%
                    arrange(desc(level))
                  )[[FacVar]]
  
  
  FacedDF <- FactorDF%>%
    mutate(!!FacVar := factor(!!sym(FacVar), FactorOrder))%>%
    arrange(desc(!!sym(FacVar)))
  
  return(FacedDF)
}
