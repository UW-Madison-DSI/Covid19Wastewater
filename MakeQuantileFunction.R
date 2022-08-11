#' Title
#'
#' @param DF 
#' @param column 
#'
#' @return
#' @export
#'
#' @examples
WindowingQuantFunc <- function(DF, column){
  mindate <- min(DF$date, na.rm = TRUE)
  maxdate <- max(DF$date, na.rm = TRUE)
  FuncWindow <- min(DF$window, na.rm = TRUE)
  FuncQuant <- min(DF$quant, na.rm = TRUE)
  
  dateTOMERGEVec <- data.frame(date = seq(mindate, maxdate, 1),
                               window = FuncWindow,
                               quant = FuncQuant)
  
  K = 3
  
  RetDF <- left_join(DF, dateTOMERGEVec)%>%
    arrange(date)%>%
    mutate(ntile = rollapply(!!sym(column), 
                             width = FuncWindow,
                             FUN = quantile, 
                             probs  = FuncQuant,
                             align = "right", 
                             na.rm=T, fill=NA, 
                             names = FALSE))%>%
    mutate(pastKavg.wwlog10 = rollmean(!!sym(column),
                                       K, align = "right",
                                       na.pad = T,
                                       na.rm=T))%>%
    filter(!is.na(population_served))
  
  return(RetDF)
}

#' Title
#'
#' @param DF 
#' @param column 
#' @param quants 
#' @param windows 
#'
#' @return
#' @export
#'
#' @examples
MakeQuantileColumns <- function(DF, column, quants, windows){
  Method_DF <- expand.grid(windows, 
                           quants, 
                           unique(DF$WWTP))
  
  colnames(Method_DF) <- c("window", "quant", "WWTP")
  
  Quantiles_DF <- DF%>%
    full_join(Method_DF, by = c("WWTP"))%>%
    split(~WWTP+window+quant)%>%
    lapply(WindowingQuantFunc, column = column)%>%
    bind_rows()
  return(Quantiles_DF)
}