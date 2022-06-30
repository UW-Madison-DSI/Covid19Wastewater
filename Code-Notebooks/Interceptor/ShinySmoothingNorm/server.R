
Server <- function(input, output, session) {
  
  NormType <- reactive({
    if(input$WastewaterNorm == "No Norm"){
      UsedVar <- input$WastewaterSignal
    }else{
      UsedVar <- paste0(input$WastewaterNorm,input$WastewaterSignal)
    }
    return(UsedVar)
  })
  
  UsedDF <- reactive({
    DF <- CreateDF("LIMSWasteData_02-09-22.csv")
  })
  
  FiltDF <- reactive({
    if(input$isOutlier){
      FiltDF <- UsedDF()%>%
        filter(Threshold == "Inliers")
    }else{
      FiltDF <- UsedDF()
    }

    FiltDF <- FiltDF%>%
      group_by(Site) %>% 
      mutate(!!paste0("Loess",NormType()) := LoessGen(!!sym(NormType()), Date, input$span))
    return(FiltDF)
  })
  
  output$plot1<-renderPlotly({

    
    BasePlot <- FiltDF()%>%
      filter(!is.na(!!sym(input$WastewaterSignal)))%>%
      ggplot(aes(x=Date, color = Site))+
      geom_point(aes(y=!!sym(NormType()), shape = Threshold), alpha = .2)+
      geom_line(aes(y=!!sym(paste0("Loess",NormType()))))
    if(input$isLog){
      BasePlot <- BasePlot+
        scale_y_log10()
    }
    
    
    ggplotly(BasePlot)
  })

  output$plotCSV<-renderPlot({
    
    if(input$isLog){
      PrepDF <- FiltDF()%>%
        mutate(!!NormType() := log(!!sym(NormType())))
    }else{
      PrepDF <- FiltDF()
    }
    
    LocSite <- c("MMSD-P11","MMSD-P18","MMSD-P2","MMSD-P7","MMSD-P8")
    Sites <- split(PrepDF, PrepDF$Site)
    SumVec <- 0
    for(i in 1:4){
      for(j in (i+1):5){
        MainDF <- Sites[[LocSite[i]]]
        SubDF <- Sites[[LocSite[j]]]
        SumVec <- SumVec + ccf(MainDF[[NormType()]],SubDF[[NormType()]],lag.max = 20, na.action = na.pass, plot=FALSE)[[1]]
      }
    }
    plot(-20:20, y=SumVec, ylab="Sum 10 CCF", xlab="Ofset")
  })
}