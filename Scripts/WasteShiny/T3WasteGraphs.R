HFGWasteDF<- reactive({
  WasteDF=HFGFrame%>%
    filter(!is.na(Site))%>%
    mutate(Site=ifelse(Site=="Madison","MadisonHFG",Site))%>%
    filter(Site %in% input$Site3)%>%
    select(Date,Site,N1,N2,PMMoV,Pct_BCoV,AVG)
  return(WasteDF)
})
LongWasteDF<- reactive({
  WasteDF=LatWasteDF%>%
    filter(!is.na(Site))%>%
    filter(Site %in% input$Site3)
  return(WasteDF)
})  
WasteDFGen <- reactive({
  WasteDF=rbind(HFGWasteDF(),LongWasteDF())%>%
    filter(N1<5e7)
  return(WasteDF)
})
LongCaseDF<- reactive({
  CaseDF=filter(LatCaseDF, Site %in% input$Site3)%>%
    select(Date,Site,Cases,Per_pos)
  return(CaseDF)
})
HFGCaseDF3 = reactive({
  CaseDF=HFGCaseDF%>%
    filter(!is.na(Site))%>%
    mutate(Site=ifelse(Site=="Madison","MadisonHFG",Site))%>%
    filter(Site %in% input$Site3)%>%
    mutate(Cases=CollectedCases,Per_pos=NA)%>%
    mutate(Cases=ifelse(Cases==-999,2.5,Cases))%>%
    select(Date,Site,Cases,Per_pos)
  return(CaseDF)
})
CaseDFGen <- reactive({
  return(rbind(HFGCaseDF3(),LongCaseDF()))
})



DFGen <- reactive({
  FullDF=full_join(CaseDFGen(),WasteDFGen(),by=c("Date","Site"))
  SecondaryThreshold=mean(pull(FullDF,input$SecondaryVars3),na.rm=T)*input$Thresholding
  FullDF=FullDF%>%
    mutate(Threshold="Low Main & Sec")%>%
    mutate(Threshold=ifelse(!!sym(input$SecondaryVars3)>SecondaryThreshold,
                            paste("High",input$SecondaryVars3),
                            paste("Low",input$SecondaryVars3)))%>%
    mutate(Threshold=ifelse(is.na(Threshold),"weird",Threshold))

  return(FullDF)
})

ToLog=c("N1","N2","PMMoV","AVG")

  T3Plot = renderPlot(width = function() 245+400*length(input$Site3),
                      height = function() 60+600,
    {
    GraphLimitsDF=DFGen()%>%
      filter(!is.na(!!sym(input$MainVars3)))
    
    GraphLimits=c(min(GraphLimitsDF$Date),max(GraphLimitsDF$Date))
    Plot1=ggplot(data=DFGen())+aes(x=!!sym(input$SecondaryVars3),
                                   y=!!sym(input$MainVars3),color = Threshold)+
          geom_point()+facet_wrap(~Site,nrow=1,scales = "free")
    Plot2=ggplot(data=DFGen())+aes(x=Date,y=!!sym(input$MainVars3),color = Threshold)+
      geom_point()+facet_wrap(~Site,nrow=1,scales = "free_x")+scale_x_date(limits=GraphLimits)
    
    if(input$MainVars3 %in% ToLog){
      Plot1=Plot1+scale_y_log10()
      Plot2=Plot2+scale_y_log10()
    }
    if(input$SecondaryVars3 %in% ToLog){
      Plot1=Plot1+scale_x_log10()
    }
    
    return(ggarrange(Plot1,Plot2,ncol=1))
    
  })