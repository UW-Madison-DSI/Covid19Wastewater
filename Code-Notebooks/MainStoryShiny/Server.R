DateOutliers = function(Df,DateEdge){
  FilteredDf <- Df%>%#Remove older data that clearly has no relationship to Cases
    mutate(WasteVar = ifelse(Date < DateEdge,NA,WasteVar))
  return(FilteredDf)
}



VarianceOutliers = function(Df,Ratio,trendlength,BinSize = 21,Threshhold=2.5){
  FilteredDf <- Df%>%#
    mutate(FlagedOutliers = IdentifyOutliers(WasteVar, Action = "Flag",
                                             Bin = BinSize, Gap = Threshhold),
           #Manual flagging that method misses due to boundary effect with binning
           FlagedOutliers = ifelse(Date == mdy("01/26/2021") | Date == mdy("01/27/2021"),
                                   TRUE, FlagedOutliers),
           OutlierVar = ifelse(FlagedOutliers, WasteVar, NA),
           #NoOutlierVar
           WasteVar = ifelse(FlagedOutliers, NA, WasteVar))
  return(FilteredDf)
}

LoessSmooth = function(Df, Span){
  LoessSmoothedDf <- Df
  LoessSmoothedDf$loessN1 = loessFit(y=(Df[["WasteVar"]]), 
                x=Df$Date, #create loess fit of the data
                span=Span, #span of .2 seems to give the best result, not rigorously chosen
                iterations=2)$fitted#2 iterations remove some bad patterns
  return(LoessSmoothedDf)
}

#SLDWidth <- 21
#scale  <- 5.028338
#shape  <- 2.332779
SLDSmooth = function(Df,Mean,StandardDeviation,SLDWidth){
  Scale = StandardDeviation^2/Mean
  Shape = Mean/Scale

  weights <- dgamma(1:SLDWidth, scale = Scale, shape = Shape)
  SLDSmoothedDF <- Df%>%
    mutate(
      SLDCases = c(rep(NA,SLDWidth-1),#eliminates cases starting values
                   rollapply(CaseVar,width=SLDWidth,FUN=weighted.mean,
                             w=weights,
                             na.rm = FALSE)))#no missing data to remove
  return(SLDSmoothedDF)
}

  


Server <- function(input, output, session) {
  
  TransformCaseData = reactive({
    ReturnedDF <- LatCaseDF%>% 
      filter(Site == input$Site)%>%
      rename(CaseVar = input$CaseSignal)%>%
        mutate(SevDay = rollmean(CaseVar, k = 3, fill = NA))
    
    if(input$SLD){
      ReturnedDF <- SLDSmooth(ReturnedDF,input$Mean,input$SD,input$SLDWidth)
    }
    return(ReturnedDF)
  })
  
  
  TransformWasteWaterData = reactive({
    ReturnedDF <- LIMSFullDF%>% 
      filter(Site == input$Site)%>%
      rename(WasteVar = input$WasteWaterSignal)
    if(input$VarianceOutliers){
      ReturnedDF <- VarianceOutliers(ReturnedDF,input$Error,input$TrendSmooth,
                                  BinSize=input$TrendSmooth,Threshhold = input$Error)
    }
    if(input$Loess){
      ReturnedDF <- LoessSmooth(ReturnedDF,input$span)
    }
    return(ReturnedDF)
    
    
  })
  
  
  TranformData = reactive({
    #joining the two data frames together
    FullDF <- full_join(TransformCaseData(),TransformWasteWaterData(), by = c("Date","Site"))
    return(FullDF)
  })
  
  
  MakePlot = reactive({
    
    WasteVarName <- input$WasteWaterSignal
    CaseVarName <- input$CaseSignal
    OutlierName <- paste(WasteVarName,"Outlier")
    loessVar <- paste0("loess",WasteVarName)
    PlotColors <- c("#F8766D", "#7393B3", "#4057A2", "#999999", "#800080", "#D6544B")
    PlotObjects <- c(WasteVarName,CaseVarName,"Seven Day MA Cases",OutlierName,"SLD Cases",loessVar)
    names(PlotColors) <- PlotObjects
    ColorRule <- scale_color_manual(values = PlotColors)

    MainDf = TranformData()
  
    FirstImpression <- MainDf%>%
      ggplot(aes(x=Date))+#Data depends on time
      labs(y="Case (#tested positve)")
      
    if(input$Cases){
      FirstImpression <- FirstImpression + 
        geom_rect(data=filter(MainDf, !is.na(CaseVar)),
                              aes(ymin=0,ymax=CaseVar,
                      xmin=Date-.25, xmax=Date+.25,
                      info=CaseVar, color = CaseVarName),
                  fill = NA,alpha=.3)+
        geom_line(aes(y=SevDay,
                      color = "Seven Day MA Cases"))
    }
    
    if(input$N1){
      FirstImpression <- FirstImpression+
        geom_line(data=filter(MainDf, !is.na(WasteVar)),
                  aes(y=MinMaxFixing(WasteVar,CaseVar),
                    color=WasteVarName,info=WasteVar))
    }
    
    
    if(input$ShowOutliers){
      FirstImpression <- FirstImpression+
        geom_point(aes(y=MinMaxFixing(OutlierVar,CaseVar,WasteVar),
                      color=OutlierName,info=OutlierVar))
    }
    
      
    if(input$SLD){
      FirstImpression <- FirstImpression+
        geom_line(data=filter(MainDf, !is.na(SLDCases)),
                  aes(y=SLDCases, 
                      color="SLD Cases",info=SLDCases))
    }
    
    if(input$Loess){
      FirstImpression <- FirstImpression+
        geom_line(data=filter(MainDf, !is.na(loessN1)),
                  aes(y=MinMaxFixing(loessN1,CaseVar,WasteVar),
                      color=loessVar,info=loessN1))
    }

    
    if(input$LogScale){
      FirstImpression <- FirstImpression+
        scale_y_log10()
    }
    FirstImpression2 = FirstImpression+
      ColorRule
    
    PlotlyVersion = ggplotly(FirstImpression2,tooltip=c("info","Date"))%>%
      add_lines(x=~Date, y=MainDf[["WasteVar"]],
                yaxis="y2", data=MainDf, showlegend=FALSE, inherit=FALSE) %>%
      layout(yaxis2 = list(#Second Axis components
        tickfont = list(size=11.7),
        titlefont=list(size=14.6),
        overlaying = "y",
        nticks = 4,
        dtick = 5e5,
        side = "right",
        title = paste(WasteVarName,"(GC/L)"),
        exponentformat = "e"
      ),
             legend=list(title=list(text=''),x = 1.15, y = 0.9),
             margin = list(l = 50, r = 75, b = 50, t = 50, pad = 4))%>%
      layout(height = 800, width = 1200)
    
    return(PlotlyVersion)
  })
  
    #
  output$plot1<-renderPlotly(
    MakePlot()
    )
  

  
}