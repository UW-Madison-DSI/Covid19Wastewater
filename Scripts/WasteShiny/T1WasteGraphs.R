filtered_data_PWaste<- reactive({
  return(filter(LatWasteDF, Site %in% input$Site))
})
filtered_data_PCov<- reactive({
  return(filter(LatCaseDF, Site %in% input$Site))
})

filtered_data_PCovRoll<- reactive({
  return(filter(LatCaseDFRoll,Site %in% input$Site))
})



minCDorms=reactive({
  return(min(filtered_data_PWaste()$Date)+input$Offset)
})

maxCDorms=reactive({
  return(max(filtered_data_PWaste()$Date)+input$Offset)
})
#Creates list of ggplots for ggarange
Make_Plots1 = reactive({
  Date_lim_long=c(min(filtered_data_PWaste()$Date),max(filtered_data_PWaste()$Date))
  plots=lapply(X=input$VarsWaste,FUN=Buildplot_gen,
               MainDF=filtered_data_PWaste(),
               Loc="Site",
               spanN=input$span,
               log_scale=(input$scale=="log"),
               DateLimits=Date_lim_long,Standards=ConfigOption())
  if(length(input$VarsTest)!=0){
    
    if(input$'7day'){
      LineVariF=filtered_data_PCovRoll()
    }else{
      LineVariF=NA
    }
    CasePlot=Buildplot_gen(
      input$VarsTest,
      MainDF=filtered_data_PCov(),
      Loc="Site",
      LineDF=LineVariF,
      DateLimits=Date_lim_long+input$Offset,
      Standards=ConfigOption(),
      AxisPos="bottom",
      LineColor="red",
      Colplot=TRUE)
    plots=c(list(CasePlot),plots)
  }
  #DormPlot=DormPlot+ylab("Percent Positive")+
  return(plots)
})

T1Plot=renderPlot(
  width = function() 245+700*length(input$Site),
  height = function() 60+300*(length(input$VarsTest)+length(input$VarsWaste)),
  {
    if((length(input$VarsTest)+length(input$VarsWaste))==0||length(input$Site)==0){
      return()
    }
    plots=Make_Plots1()
    #Add Site labels
    #add x axis labels
    if(length(input$VarsTest)!=0){
      T=2
      if(length(input$VarsWaste)==0){
        T=1
      }
    }else{
      T=1
    }
    plots[[1]]=plots[[1]] + Header_theme(ConfigOption())+
      theme(axis.text.x = element_text(size = ConfigOption()$XAxisLabSiz, colour = "black"),
            plot.margin = unit(c(.5,0,1,0), "cm"))
    plots[[T]]=plots[[T]] + Second_theme(ConfigOption())
    OvarLn=length(input$VarsWaste)
    return(annotate_figure(top=text_grob("UW dorms Wastewater Surveillance",size=25),ggarrange(plotlist=plots,ncol =1,align="v",heights=c(1.2,rep(1,times = OvarLn)))))
  })