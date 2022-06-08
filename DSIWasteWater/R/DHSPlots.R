#' CreateHeatMaps
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF 
#' @param ToMerge if true we remove the lower labels
#' @export
#' @return faceted ggplot
CreateHeatMaps <- function(DF, ToMerge = FALSE){#, 
  CatagoryColors <- c("#0571b0","#92c5de", "#979797","WHITE","#f4a582","#ca0020")
  BarGridSmoothRaw <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-days_elapsed/2,xmax=date+days_elapsed/2,
                  ymin=0,
                  ymax = 10,fill = Catagory))+
    facet_grid(Method ~ WWTP)+
    scale_fill_manual(values = CatagoryColors)
  if(ToMerge){
    BarGridSmoothRaw <- BarGridSmoothRaw+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
  return(BarGridSmoothRaw)
}



#' ConfMatrix
#' 
#' creates a confusion matrix from data long format
#'
#' @param DF 
#' @param Cat The column with the values of the methods 
#' @param x The first method to compare
#' @param y The second method to compare
#' @export
#' @return a ggplot object of the confusion matrix
ConfMatrix <- function(DF,Cat,x,y){
  RetPlt <- DF%>%
    filter(Method %in% c(x,y))%>%
    select(WWTP,date,Method,Catagory)%>%
    filter(WWTP != "Portage WWTF"  & WWTP != "Cedarburg WWTF")%>%
    pivot_wider(id_cols=c(WWTP,date),names_from = Method, values_from = !!sym(Cat))%>%
    group_by(!!sym(x),!!sym(y))%>%
    summarise(n = n())%>%
    filter(!is.na(!!sym(y)))%>%
    ggplot(aes(x=!!sym(x),y=!!sym(y)))+
    geom_tile(aes(fill = n))+
    scale_fill_gradient(low="blue", high="red")
  return(RetPlt)
}

#' Title
#'
#' @param GGObj 
#' @param GGfunc 
#' @param YName 
#' @param YVal 
#'
#' @return
#'
#' @examples
Abstract_PlotAdd <- function(GGObj, GGfunc, YName, YVal){
  if(YName != YVal){
    YName <- sym(YName)
  }
  RetObj <- GGObj+
    GGfunc(aes(y = !!sym(YVal), color = !!YName))
  return(RetObj)
}


#' Title
#'
#' @param DF 
#' @param xVal 
#' @param PointVal 
#' @param LineVal 
#' @param PointName 
#' @param LineName 
#' @param ToMerge 
#'
#' @return
#' @export
#'
#' @examples
WastePlot <- function(DF, xVal, PointVal, LineVal,PointName = NULL, LineName = NULL, ToMerge = FALSE){
  RetPlot <- DF%>%
    ggplot( aes(x = !!sym(xVal)))
  if(!is.na(PointName)){
    RetPlot <- RetPlot%>%
      Abstract_PlotAdd(geom_point, PointName, PointVal)
  }
  if(!is.na(LineName)){
    RetPlot <- RetPlot%>%
      Abstract_PlotAdd(geom_line, LineName, LineVal)
  }
  if(ToMerge){
    RetPlot <- RetPlot+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )
  }
  return(RetPlot)
}
