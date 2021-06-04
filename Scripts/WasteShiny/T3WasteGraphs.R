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
    select(Date,Site,Cases,Per_pos)
  return(CaseDF)
})
CaseDFGen <- reactive({
  return(rbind(HFGCaseDF3(),LongCaseDF()))
})



DFGen <- reactive({
  FullDF=full_join(CaseDFGen(),WasteDFGen(),by=c("Date","Site"))
  MainThreshold     =mean(pull(FullDF,input$MainVars3),na.rm=T)*input$MainThreshold
  SecondaryThreshold=mean(pull(FullDF,input$SecondaryVars3),na.rm=T)*input$SecondaryThreshold
  FullDF=FullDF%>%
    mutate(Threshold="Low Main & Sec")%>%
    mutate(Threshold=ifelse(!!sym(input$SecondaryVars3)>SecondaryThreshold,"High Sec",Threshold))%>%
    mutate(Threshold=ifelse(!!sym(input$MainVars3)>MainThreshold,"High Main",Threshold))%>%
    mutate(Threshold=ifelse(!!sym(input$MainVars3)>MainThreshold&
                            !!sym(input$SecondaryVars3)>SecondaryThreshold,"High Main & Sec",Threshold))%>%
    mutate(Threshold=ifelse(is.na(Threshold),"weird",Threshold))

  return(FullDF)
})

ToLog=c("N1","N2","PMMoV","AVG")

  T3Plot = renderPlot(width = function() 245+700,
                      height = function() 60+300,
    {
    Plot1=ggplot(data=DFGen())+aes(x=!!sym(input$SecondaryVars3),
                                   y=!!sym(input$MainVars3),color = Threshold)+
          geom_point()+facet_wrap(~Site,nrow=1)
    Plot2=ggplot(data=DFGen())+aes(x=Date,y=!!sym(input$MainVars3),color = Threshold)+
      geom_point()+facet_wrap(~Site,nrow=1,scales = "free_x")
    
    if(input$MainVars3 %in% ToLog){
      Plot1=Plot1+scale_y_log10()
      Plot2=Plot2+scale_y_log10()
    }
    if(input$SecondaryVars3 %in% ToLog){
      Plot1=Plot1+scale_x_log10()
    }
    
    return(ggarrange(Plot1,Plot2,ncol=1))
    
  })