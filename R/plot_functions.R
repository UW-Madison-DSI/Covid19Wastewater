#' Create representative plot of the DHS analysis
#' 
#' createRegressionAnalysis_Plot uses RegDF to create the top set of plots showing the 
#' predictions at each time period. 
#' It uses BaseDF to create the lower plot that shows what the regressed data
#' looks like
#'
#' @param RegDF data frame containing the regression analysis
#' @param BaseDF The data frame containing the raw data
#' @param FacGridFormula The formula we wish to facet the heat maps with
#' @param PointVal The point columns we want to plot
#' @param LineVal The Line columns we want to plot
#' @param nbreak The number of plots in each row
#' @param IsLong Controls whether the plot is wide or long
#'
#' @return a ggplot of the heat map of each method and the underlying data
#' @export
#'
#' @examples
#' 
#' data(example_data, package = "DSIWastewater")
#' example_reg_table <- buildRegressionEstimateTable(example_data)
#' createRegressionAnalysis_Plot(example_reg_table, example_data)
createRegressionAnalysis_Plot <- function(RegDF, BaseDF, 
                             FacGridFormula = Method ~ WWTP,
                             PointVal = "sars_cov2_adj_load_log10", 
                             LineVal = NULL, 
                             nbreak = 3,
                             IsLong = TRUE
                             ){
  if(IsLong){
    Mainbreak <- as.character(FacGridFormula)[3]
    SubBreak <- as.character(FacGridFormula)[2]
    facetFormula = paste("Data ~", Mainbreak)
  }else{
    Mainbreak <- as.character(FacGridFormula)[2]
    SubBreak <- as.character(FacGridFormula)[3]
    facetFormula = paste(Mainbreak, "~ Data")
  }
  
  CatagoryColors <- c("major decrease" = "#0571b0", "moderate decrease" = "#92c5de",
                      "fluctuating" = "#979797", "no change" = "WHITE", 
                      "moderate increase" = "#f4a582", "major increase" = "#ca0020")
  
  BarGridSmoothRaw <- RegDF%>%
    
    split(.,.[[Mainbreak]])%>%
    
    lapply(CreateHeatMaps_Plot, FacGridFormula, "Catagory", CatagoryColors)
  
  
  
  
  Gplt <- BaseDF%>%
    
    mutate(Data = "Base Data")%>%
    
    filter(!!sym(Mainbreak) %in% unique(RegDF[[Mainbreak]]))%>%
    
    split(.,.[[Mainbreak]])%>%
    
    lapply(createWasteGraph_Plot, 
           "date", 
           PointVal = PointVal, 
           LineVal = LineVal,
           facetFormula = facetFormula)
  
  
  methodsUsed <- length(uniqueVal(SubBreak, RegDF))
  
  SavePlot <- orderAndZipListsOfPlots_Plot(BarGridSmoothRaw,Gplt,
                                             ratA = methodsUsed, 
                                             nbreak = nbreak,
                                             IsLong = IsLong)
  return(SavePlot)
}

#' Take two list of plots and combine them into one 3 col long plot
#'
#' @param top_plot_list Lists of plots that get added on top, generically the 
#' rectangle plot of method prediction. Needs to be the same length as bot_plot_list
#' @param bot_plot_list  List of plots to be combined on the bottom 
#' @param ratA The proportion the top plot should be.
#' @param ratB The proportion the bot plot should be.
#' @param nbreak Where the plot should be faceted
#' @param IsLong Controls whether the plot is wide or long
#'
#' @return a ggplot
orderAndZipListsOfPlots_Plot <- function(top_plot_list, bot_plot_list, ratA=3,
                                         ratB=1, nbreak = 3, IsLong = TRUE){
  stopifnot(length(top_plot_list) == length(bot_plot_list))
  
  EffectiveNbreak = min(nbreak, length(top_plot_list))
  if(IsLong){
    Height = c(ratA, ratB)
    Width = NULL
    nrow = NULL
    ncol = EffectiveNbreak
  }else{
    Height = NULL
    Width = c(ratB, ratA)
    nrow = EffectiveNbreak
    ncol = NULL
  }
  TitleRemove<- theme(axis.title.y = element_blank())
  topStripRemove <- theme(strip.background.x = element_blank(),
                          strip.text.x = element_blank())
  sideRemove<- theme(strip.background.y = element_blank(),
                     strip.text.y = element_blank())
  XAxisRemove <- theme(axis.title.x = element_blank(),
                       axis.text.x = element_blank(),
                       axis.ticks.x = element_blank())
  
  if(length(top_plot_list) == 1){
    botElement <- bot_plot_list[[1]]
    topElement <- top_plot_list[[1]]
    if(IsLong){
      topElement <- topElement + XAxisRemove
      botElement <- botElement + topStripRemove
      
      compPlot <- (topElement / botElement)
    }else{
      botElement <- botElement + sideRemove
      compPlot <- (botElement | topElement)
    }
    retPlot <- compPlot + plot_layout(heights = Height,
                                           widths = Width)
    return(retPlot)
  }
  
  
  
  RetList <- list()
  
  ele_list_length <- length(top_plot_list)
  for(i in 1:ele_list_length){
    botElement <- bot_plot_list[[i]]
    topElement <- top_plot_list[[i]]
    if(i %% EffectiveNbreak != 1){
      
      if(IsLong){
        botElement <- botElement + TitleRemove
      }else{
        topElement <- topElement + topStripRemove
        botElement <- botElement + topStripRemove
      }
      
    }
    
    if(i %% EffectiveNbreak != 0 && i != ele_list_length){
      if(IsLong){
        topElement <- topElement + sideRemove
        botElement <- botElement + sideRemove
      }else{
        topElement <- topElement + XAxisRemove
        botElement <- botElement + XAxisRemove
      }
    }
    
    if(IsLong){
      topElement <- topElement + XAxisRemove
      botElement <- botElement + topStripRemove
      
      compPlot <- (topElement / botElement)
    }else{
      botElement <- botElement + sideRemove
      compPlot <- (botElement | topElement)
    }
    RetList[[i]] <- compPlot + plot_layout(heights = Height,
                                   widths = Width)
  }
  retPlot <- wrap_plots(RetList) + plot_layout(guide="collect", 
                                               ncol = ncol,
                                               nrow = nrow)
  return(retPlot)
}



#' CreateHeatMaps_Plot
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF The DF used to create the Heatmap
#' @param FacGridFormula how we wan to facet the heatmap
#' @param FillFac the name of the category method
#' @param CatagoryColors The color scheme used
#'
#' @return faceted ggplot heatmap
CreateHeatMaps_Plot <- function(DF, FacGridFormula, FillFac, CatagoryColors){#, 
  BarGridSmoothRaw <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-days_elapsed/2,xmax=date+days_elapsed/2,
                  ymin=0,
                  ymax = 10,fill = !!sym(FillFac)),
              na.rm=TRUE)+
    facet_grid(FacGridFormula)+
    scale_fill_manual(values = CatagoryColors)

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
    GGfunc(aes(y = !!sym(YVal), color = !!YcolorName), na.rm = TRUE, size = .5)
  return(RetObj)
}


#' Wastewater graphic
#'
#' @param DF DF containing wastewater measurements specified in the remaining params
#' @param xVal name of x variable, normally close to "Date"
#' @param PointVal the discrete measurements
#' @param LineVal the continuous measurements
#' @param facetFormula formula of how to facet the plot
#'
#' @return a ggplot object with points with lables for each PointVal and a lines for each LineVal
createWasteGraph_Plot <- function(DF, xVal, PointVal = NULL,
                                  LineVal = NULL,
                                  facetFormula = "Data ~ WWTP"){
  RetPlot <- DF%>%
    ggplot( aes(x = !!sym(xVal)))
  
  if(!is.null(PointVal)){
    for (ele in PointVal) {
      RetPlot <- RetPlot%>%
        Abstract_PlotAdd(geom_point, ele)
    }
  }
  
  if(!is.null(LineVal)){
    for (ele in LineVal) {
      RetPlot <- RetPlot%>%
        Abstract_PlotAdd(geom_line, ele)
    }
  }
  
  RetPlot <- RetPlot+
    facet_grid(as.formula(facetFormula))+
    scale_x_date(date_labels = "%b %y")
  
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
createConfMatrix_Plot <- function(DF,x,y, Cat="Catagory"){
  RetPlt <- DF%>%
    filter(Method %in% c(x,y))%>%
    select(WWTP,date,Method,Catagory)%>%
    filter(WWTP != "Portage WWTF"  & WWTP != "Cedarburg WWTF")%>%
    pivot_wider(id_cols=c(WWTP, date),names_from = Method, values_from = !!sym(Cat))%>%
    group_by(!!sym(x),!!sym(y))%>%
    summarise(n = n())%>%
    filter(!is.na(!!sym(y)), !is.na(!!sym(x)))%>%
    ggplot(aes(x=!!sym(x),y=!!sym(y)))+
    geom_tile(aes(fill = n), na.rm=TRUE)+
    scale_fill_gradient(low="blue", high="red")+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  return(RetPlt)
}



#' createMethodCompareBar_Plot: Compare Regression analysis methods
#'
#' @param DF data frame containing all the analysis for every method
#' @param Method factor column for each type of regression done
#' @param Cat Category column that the regression analysis is store
#'
#' @return ggplot object
createMethodCompareBar_Plot <- function(DF,Method = "Method",Cat="Catagory"){
  DF%>%
    group_by(!!sym(Method),!!sym(Cat))%>%
    summarise(n = n())%>%
    ggplot(aes(x=!!sym(Cat),y=n))+
    geom_col(aes(fill=!!sym(Method)),position = "dodge", na.rm=TRUE)+ 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}



#' Create graphics showing flags as vertical lines
#'
#' @param MainDF DF used for the non geom_vline calls
#' @param FlagDF DF used for ploting the flag vertical lines
#' @param Flag1 name of first flag
#' @param Flag2 name of second flag, if only one flag leave NULL
#' @param xVal column to use as x var
#' @param PointVal list of columns to be shown as points
#' @param LineVal list of columns to be shown as lines
#' @param facetFormula formula object to be fed into geom_facet
#'
#' @return ggplot object of scatter, line and vertical lines
#' @export
createFlagGraph_plot <- function(MainDF, FlagDF, 
                                 Flag1 = NULL, 
                                 Flag2 = NULL, 
                                 xVal = "date",
                                 PointVal = NULL,
                                 LineVal = NULL,
                                 facetFormula = " ~ Site"){
  #use already existing plot code to get points and lines
  StartPlot <- createWasteGraph_Plot(MainDF, 
                                     xVal = xVal,
                                     PointVal = PointVal,
                                     LineVal = LineVal,
                                     facetFormula = facetFormula)
  #if flag2 is not null then we need a both line
  if(!is.null(Flag2)){
    #adding to the ggplot obj we created in last step
    StartPlot <- StartPlot+
      #add line where there is a flag1 but not a flag2
      geom_vline(aes(xintercept = !!sym(xVal), color = Flag1),
                 data = filter(FlagDF, !!sym(Flag1) == 1, !!sym(Flag2) == 0))+
      #add line where there is a flag2 but not a flag2
      geom_vline(aes(xintercept = !!sym(xVal), color = Flag2),
                 data = filter(FlagDF, !!sym(Flag1) == 0, !!sym(Flag2) == 1))+
      #add line where there is both a flag1 and a flag2
      geom_vline(aes(xintercept = !!sym(xVal), color = "Both"),
                 data = filter(FlagDF, !!sym(Flag1) == 1, !!sym(Flag2) == 1))
  }else{
    #othwewise just add flags where Flag1 is 1
    StartPlot <- StartPlot+
      geom_vline(aes(xintercept = !!sym(xVal), color = Flag1),
                 data = filter(FlagDF, !!sym(Flag1) == 1))
  }
  return(StartPlot)
}
