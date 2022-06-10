#' Create representative plot of the DHS analysis
#'
#' @param RegDF DF containing the regression analysis
#' @param BaseDF The DF containing the raw data
#' @param FacGridFormula The formula we wish to facet the heatmaps with
#' @param SiteName The column names for Site
#' @param PointName The point columns we want to plot
#' @param LineName The line columns we want to plot
#'
#' @return a ggplot of the heatmap of each method and the underlying data
#' @export
#'
#' @examples
DHSTopLevelPlots <- function(RegDF,BaseDF, FacGridFormula = Method ~ WWTP,
                             SiteName = "WWTP", PointName = "sars_cov2_adj_load_log10",  
                             LineName = NULL){
  
  CatagoryColors <- c("major decrease" = "#0571b0", "moderate decrease" = "#92c5de",
                      "fluctuating" = "#979797", "no change" = "WHITE", 
                      "moderate increase" = "#f4a582", "major increase" = "#ca0020")
  
  BarGridSmoothRaw <- RegDF%>%
    
    FactorVecByNumPoints(SiteName,"Catagory")%>%
    
    CreateHeatMaps(FacGridFormula, "Catagory", CatagoryColors,ToMerge=TRUE)
  
  
  Gplt <- BaseDF%>%
    FactorVecByNumPoints("WWTP","sars_cov2_adj_load_log10")%>%
    
    mutate(Data = "Data")%>%
    
    filter(!!sym(SiteName) %in% unique(RegDF[[SiteName]]))%>%
    
    WastePlot("date", PointName,  LineName, PointName,  LineName, ToMerge = TRUE)
  
  
  methodsUsed <- length(uniqueVal(as.character(Formula)[2], RegDF))

  SavePlot <- BarGridSmoothRaw/Gplt + plot_layout(heights = c(methodsUsed, 1))
  
  return(SavePlot)
}




#' CreateHeatMaps
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF The DF used to create the Heatmap
#' @param FacGridFormula how we wan to facet the heatmap
#' @param FillFac the name of the catagory method
#' @param CatagoryColors The color scheme used
#' @param ToMerge if true we remove the lower labels
#'
#' @export
#' @return faceted ggplot heatmap
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
#' @param DF data frame containing results of DHS analysis
#' @param Cat The column with the values of the methods 
#' @param x The first method to compare
#' @param y The second method to compare
#'
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

#' adds a ggplot component
#'
#' @param GGObj a ggplot object we are adding to
#' @param GGfunc what gg type object used
#' @param YcolorName name of the color, either a factor or a string
#' @param YVal name of the y variable used
#'
#' @return GGObj with the appended graphic
#'
#' @examples
Abstract_PlotAdd <- function(GGObj, GGfunc, YcolorName, YVal){
  if(YcolorName != YVal){
    YcolorName <- sym(YcolorName)
  }
  RetObj <- GGObj+
    GGfunc(aes(y = !!sym(YVal), color = !!YcolorName))
  return(RetObj)
}


#' Waste Water graphic
#'
#' @param DF DF containing waste water measurements specified in the remaining params
#' @param xVal name of x variable, normally close to "Date"
#' @param PointVal the discrete measurement
#' @param LineVal the continuous measurement
#' @param PointName the label of PointVal
#' @param LineName the label of LineVal
#' @param ToMerge remove facet info if true. be careful that the to plots have same ordering
#'
#' @return a ggplot object
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
