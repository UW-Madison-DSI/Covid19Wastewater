#' Title
#'
#' @param RegDF 
#' @param BaseDF 
#' @param FacGridFormula 
#' @param SiteName 
#' @param PointName 
#' @param LineName 
#'
#' @return
#' @export
#'
#' @examples
DHSTopLevelPlots <- function(RegDF,BaseDF, FacGridFormula = Method ~ WWTP,
                             SiteName = "WWTP", PointName = "sars_cov2_adj_load_log10",  
                             LineName = NULL){
  
  CatagoryColors <- c("#92c5de", "#979797","WHITE","#f4a582","#ca0020")
  BarGridSmoothRaw <- RegDF%>%#"#0571b0",
    
    FactorVecByNumPoints(SiteName,"Catagory")%>%
    
    CreateHeatMaps(FacGridFormula, "Catagory", CatagoryColors,ToMerge=TRUE)
  
  
  Gplt <- BaseDF%>%
    FactorVecByNumPoints("WWTP","sars_cov2_adj_load_log10")%>%
    
    mutate(Data = "Data")%>%
    
    filter(!!sym(SiteName) %in% unique(RegDF[[SiteName]]))%>%
    
    WastePlot("date", PointName,  LineName, PointName,  LineName, ToMerge = TRUE)
  
  SavePlot <- BarGridSmoothRaw/Gplt + plot_layout(heights = c(2, 1))
  
  return(SavePlot)
}




#' CreateHeatMaps
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF 
#' @param ToMerge if true we remove the lower labels
#' @export
#' @return faceted ggplot
CreateHeatMaps <- function(DF, FacGridFormula, FillFac, CatagoryColors, ToMerge = FALSE){#, 
  BarGridSmoothRaw <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-days_elapsed/2,xmax=date+days_elapsed/2,
                  ymin=0,
                  ymax = 10,fill = !!sym(FillFac)))+
    facet_grid(FacGridFormula)+
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
  if(!is.null(PointName)){
    RetPlot <- RetPlot%>%
      Abstract_PlotAdd(geom_point, PointName, PointVal)
  }
  if(!is.null(LineName)){
    RetPlot <- RetPlot%>%
      Abstract_PlotAdd(geom_line, LineName, LineVal)
  }
  RetPlot <- RetPlot+
    facet_grid(~WWTP)+
    scale_x_date(date_labels = "%b %y")
  if(ToMerge){
    RetPlot <- RetPlot+
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )
  }
  return(RetPlot)
}
