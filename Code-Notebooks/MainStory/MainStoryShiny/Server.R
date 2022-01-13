DateOutliers = function(Df,DateEdge){
  FilteredDf <- Df%>%#Remove older data that clearly has no relationship to Cases
    mutate(N1 = ifelse(Date < DateEdge,NA,N1))
  return(FilteredDf)
}

VarianceOutliers = function(Df,Ratio,trendlength){
  FilteredDf <- Df%>%
    mutate(SmoothN1=rollapply(data = N1, width = trendlength, FUN = median, 
                              na.r = TRUE,fill=NA),#Finding very smooth version of the data with no outliers
           SmoothN1=ifelse(is.na(SmoothN1),N1,SmoothN1),#Fixing issue where rollapply fills NA on right border
           LargeError=N1>Ratio*SmoothN1,#Calculating error Limits
           N1=ifelse(LargeError,SmoothN1,N1))%>%#replacing data points that variance is to large
    select(-SmoothN1,-LargeError)#Removing unneeded calculated columns
  return(FilteredDf)
}

LoessSmooth = function(Df, Span){
  LoessSmoothedDf <- Df
  LoessSmoothedDf$loessN1 = loessFit(y=(Df$N1), 
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
                   rollapply(Cases,width=SLDWidth,FUN=weighted.mean,
                             w=weights,
                             na.rm = FALSE)))#no missing data to remove
  return(SLDSmoothedDF)
}


Server <- function(input, output, session) {
  
  
  TranformData = reactive({
    
    if(input$OutliersDate){
      FullDF2 <- DateOutliers(FullDF,input$OutliersDate2)
    }else{
      FullDF2 <- FullDF
    }
    
    if(input$VarianceOutliers){
      FullDF3 <- VarianceOutliers(FullDF2,input$Error,input$TrendSmooth)
    }else{
      FullDF3 <- FullDF2
    }
    
    if(input$PerPos){
      FullDF4 <-FullDF3%>%
        mutate(CasesRoll = c(rep(NA,6),#eliminates cases starting values
                         rollapply(Cases,width=7,FUN=sum,
                                   na.rm = FALSE)),
               TestsRoll = c(rep(NA,6),#eliminates cases starting values
                             rollapply(Tests,width=7,FUN=sum,
                                       na.rm = FALSE)),
               Cases = CasesRoll/TestsRoll)
    }else{
      FullDF4 <-FullDF3
    }
    
    if(input$SLD){
      FullDF5 <- SLDSmooth(FullDF4,input$Mean,input$SD,input$SLDWidth)
    }else{
      FullDF5 <- FullDF4
    }
    
    if(input$Loess){
      FullDF6 <- LoessSmooth(FullDF5,input$span)
    }else{
      FullDF6 <- FullDF5
    }
    vars <- c("N1","Cases")
    FullDF7 <- NoNa(FullDF6,vars)%>%
    return(FullDF7)
    
    })
  
  
  MakePlot = reactive({
    
  MainDf = TranformData()
  
  if(input$MinMax){
    ExtreameCases <- c(min(MainDf$Cases,na.rm=TRUE),max(MainDf$Cases,na.rm=TRUE))
    ExtreameN1 <- c(min(MainDf$N1,na.rm=TRUE),max(MainDf$N1,na.rm=TRUE))
  }else{
    ExtreameCases <- NA
    ExtreameN1 <- NA
    
  }
    FirstImpression <- MainDf%>%
      ggplot(aes(x=Date))+#Data depends on time
      labs(y="variable min max normalized")
      
    if(input$Cases){
      FirstImpression <- FirstImpression+
        geom_line(aes(y=MinMaxNormalization(Cases,ExtreameValues = ExtreameCases), 
                      color="Cases",info=Cases))
    }
    if(input$N1){
      FirstImpression <- FirstImpression+
        geom_line(aes(y=MinMaxNormalization(N1 , ExtreameValues = ExtreameN1),
                    color="N1",info=N1))
    }

      
    
    if(input$SLD){
      FirstImpression <- FirstImpression+
        geom_line(aes(y=MinMaxNormalization(SLDCases,ExtreameValues = ExtreameCases), 
                      color="SLDCases",info=SLDCases))
    }
    if(input$Loess){
      FirstImpression <- FirstImpression+
        geom_line(aes(y=MinMaxNormalization(loessN1 , ExtreameValues = ExtreameN1),
                      color="loessN1",info=loessN1))
    }
    if(input$LogScale){
      FirstImpression <- FirstImpression+
        scale_y_log10()
    }
    FirstImpression = FirstImpression+
      ColorRule
    
    PlotlyVersion = ggplotly(FirstImpression,tooltip=c("info","Date"))
    return(PlotlyVersion)
  })
  
    #
  output$plot1<-renderPlotly(
    MakePlot()
    )
  

  
}