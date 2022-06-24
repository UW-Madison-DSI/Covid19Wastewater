#' Create representative plot of the DHS analysis
#'
#' @param RegDF DF containing the regression analysis
#' @param BaseDF The DF containing the raw data
#' @param FacGridFormula The formula we wish to facet the heat maps with
#' @param PointVal The point columns we want to plot
#' @param LineVal The Line columns we want to plot
#' @param SiteName The column names for Site
#'
#' @return a ggplot of the heat map of each method and the underlying data
#' @export
#'
#' @examples
#' 
#' data(example_data, package = "DSIWasteWater")
#' example_reg_table <- buildRegressionEstimateTable(example_data)
#' createDHSMethod_Plot(example_reg_table, example_data)
createDHSMethod_Plot <- function(RegDF,BaseDF, FacGridFormula = Method ~ WWTP,
                             SiteName = "WWTP", 
                             PointVal = "sars_cov2_adj_load_log10", 
                             LineVal = NULL
                             ){
  
  CatagoryColors <- c("major decrease" = "#0571b0", "moderate decrease" = "#92c5de",
                      "fluctuating" = "#979797", "no change" = "WHITE", 
                      "moderate increase" = "#f4a582", "major increase" = "#ca0020")
  
  BarGridSmoothRaw <- RegDF%>%
    
    FactorVecByNumPoints(SiteName,"Catagory")%>%
    
    CreateHeatMaps_Plot(FacGridFormula, "Catagory", CatagoryColors,ToMerge=TRUE)
  
  
  
  
  Gplt <- BaseDF%>%
    FactorVecByNumPoints("WWTP","sars_cov2_adj_load_log10")%>%
    
    mutate(Data = "Data")%>%
    
    filter(!!sym(SiteName) %in% unique(RegDF[[SiteName]]))%>%
    
    createWasteGraph_Plot("date", PointVal = PointVal, LineVal = LineVal, ToMerge = TRUE)
  
  
  methodsUsed <- length(uniqueVal(as.character(FacGridFormula)[2], RegDF))

  SavePlot <- BarGridSmoothRaw/Gplt + plot_layout(heights = c(methodsUsed, 1))
  
  return(SavePlot)
}

#' CreateHeatMaps_Plot
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF The DF used to create the Heatmap
#' @param FacGridFormula how we wan to facet the heatmap
#' @param FillFac the name of the catagory method
#' @param CatagoryColors The color scheme used
#' @param ToMerge if true we remove the lower labels
#'
#' @return faceted ggplot heatmap
CreateHeatMaps_Plot <- function(DF, FacGridFormula, FillFac, CatagoryColors, ToMerge = FALSE){#, 
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

#' adds a ggplot component
#'
#' @param GGObj a ggplot object we are adding to
#' @param GGfunc what gg type object used
#' @param YcolorName name of the color, either a factor or a string
#' @param YVal name of the y variable used
#'
#' @return GGObj with the appended graphic
Abstract_PlotAdd <- function(GGObj, GGfunc, YVal, YcolorName = NULL){
  
  if(is.null(YcolorName)){
    YcolorName <- YVal
  }else{
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
#' @param ToMerge remove facet info if true. be careful that the to plots have same ordering
#' @param PointValVec the discrete measurements
#' @param LineValVec the continuous measurements
#'
#' @return a ggplot object with points with lables for each PointValVec and a lines for each LineValVec
createWasteGraph_Plot <- function(DF, xVal, PointValVec = NULL, LineValVec = NULL, ToMerge = FALSE){
  RetPlot <- DF%>%
    ggplot( aes(x = !!sym(xVal)))
  
  if(!is.null(PointValVec)){
    for (ele in PointValVec) {
      RetPlot <- RetPlot%>%
        Abstract_PlotAdd(geom_point, ele)
    }
  }
  
  if(!is.null(LineValVec)){
    for (ele in LineValVec) {
      RetPlot <- RetPlot%>%
        Abstract_PlotAdd(geom_line, ele)
    }
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


#' createConfMatrix_Plot
#' 
#' creates a confusion matrix from data long format
#'
#' @param DF data frame containing results of DHS analysis
#' @param Cat The column with the values of the methods 
#' @param x The first method to compare
#' @param y The second method to compare
#' @return a ggplot object of the confusion matrix
createConfMatrix_Plot <- function(DF,Cat,x,y){
  RetPlt <- DF%>%
    filter(Method %in% c(x,y))%>%
    select(WWTP,date,Method,Catagory)%>%
    filter(WWTP != "Portage WWTF"  & WWTP != "Cedarburg WWTF")%>%
    pivot_wider(id_cols=c(WWTP, date),names_from = Method, values_from = !!sym(Cat))%>%
    group_by(!!sym(x),!!sym(y))%>%
    summarise(n = n())%>%
    filter(!is.na(!!sym(y)))%>%
    ggplot(aes(x=!!sym(x),y=!!sym(y)))+
    geom_tile(aes(fill = n))+
    scale_fill_gradient(low="blue", high="red")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(RetPlt)
}
