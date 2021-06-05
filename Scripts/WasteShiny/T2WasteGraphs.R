#Filters data for right Sites and variables
HFGCaseDataFil = reactive({
  CaseDF=HFGCaseDF%>%
    filter(!is.na(Site))%>%
    filter(Site %in% input$Site2)
  return(CaseDF)
})

HFGCaseDataFil7day = reactive({
  CaseRollDF=HFGCaseDFRoll%>%
    filter(!is.na(Site))%>%
    filter(Site %in% input$Site2)
  return(CaseRollDF)
})
HFGCaseDataNoNeg  = reactive({
  TheData=HFGCaseDataFil()%>%
    mutate(Small3=ifelse(EpisodeCases==-999,"Range 1-5","Normal Values"))%>%
    mutate(Small=ifelse(CollectedCases==-999,"Range 1-5","Normal Values"))%>%
    mutate(Small2=ifelse(ConfirmedCases==-999,"Range 1-5","Normal Values"))%>%
    filter(!is.na(Small))%>%
    mutate(EpisodeCases=ifelse(EpisodeCases==-999,2.5,EpisodeCases))%>%
    mutate(CollectedCases=ifelse(CollectedCases==-999,2.5,CollectedCases))%>%
    mutate(ConfirmedCases=ifelse(ConfirmedCases==-999,2.5,ConfirmedCases))
  return(TheData)
})

filtered_data_P<- reactive({
  WasteDF=HFGFrame%>%
    filter(!is.na(Site))%>%
    filter(Site %in% input$Site2)
  return(WasteDF)
})


#Data of the means of each day
Data_mean_var = reactive({
  HFGSiteMean=filtered_data_P()%>%
    group_by(Date,Site)%>%
    summarise(N1=exp(mean(log(N1),na.rm = T)),
              N2=exp(mean(log(N2),na.rm = T)),
              PMMoV=exp(mean(log(PMMoV),na.rm = T)),
              Pct_BCoV=exp(mean(log(Pct_BCoV),na.rm = T)))
})
#Data of the means of each Filter
Filter_mean_var = reactive({
  HFGFilterMean=filtered_data_P()%>%
    group_by(Date,Site,`Filter replicates`)%>%
    summarise(N1=exp(mean(log(N1),na.rm = T)),
              N2=exp(mean(log(N2),na.rm = T)),
              PMMoV=exp(mean(log(PMMoV),na.rm = T)),
              Pct_BCoV=exp(mean(log(Pct_BCoV),na.rm = T)))
})
#Putting data in reactive shell for possible additions
DF_Week = reactive({
  return(HFGDateRangeDF)
})
minCHFG=reactive({
  return(min(HFGCaseDF$Date))
})

maxCHFG=reactive({
  return(max(HFGCaseDF$Date))
})

#Creates list of ggplots for ggarange
Make_Plots2 = reactive({
  Date_lim_HFG=c(min(filtered_data_P()$Date)-14,max(filtered_data_P()$Date)+14)
  if(length(input$Vars2Waste)!=0){
    if(input$Means==T){
      MeanVDF=Filter_mean_var()
    }else{
      MeanVDF=NA
    }
    if(input$Line2==T){
      LineVDF=Data_mean_var()
    }
    else{
      LineVDF=NA
    }
    plots=lapply(X=input$Vars2Waste,FUN=Buildplot_gen,
                 MainDF=filtered_data_P(),
                 Loc="Site",
                 log_scale=(input$scale2=="log"),
                 ColorType="Filter replicates",
                 LineDF=LineVDF,
                 MeanDF=MeanVDF,
                 WeekDF=DF_Week(),
                 DateLimits=Date_lim_HFG,
                 RMOutliers=("Remove Outliers" %in% input$Outlier),
                 Standards=ConfigOption(),
                 Xfreq="14 days")
  }else{plots=NA}
  if(length(input$Vars2Cas)!=0){
    if(input$'7day2'){
      LineVariF=HFGCaseDataFil7day()
    }
    else{
      LineVariF=NA
    }
    CasePlot=lapply(X=input$Vars2Cas,FUN=Buildplot_gen,
                    MainDF=HFGCaseDataNoNeg(),
                    ColorType="Small",
                    Loc="Site",
                    LineDF=LineVariF,
                    WeekDF=DF_Week(),
                    DateLimits=Date_lim_HFG+input$Offset2,
                    Standards=ConfigOption(),
                    Xfreq="14 days",
                    AxisPos="bottom",
                    LineColor="red",
                    norm=ifelse(input$PopNorm,"Population",NA),
                    Colplot=T)
    plots=c(CasePlot,plots)
  }
  
  return(plots)
})
#plots N1GC

T2Plot = renderPlot(
  width = function() 245+400*length(input$Site2),
  height = function() 60+300*(length(input$Vars2Cas)+length(input$Vars2Waste)),
  {
    NumWasteG=length(input$Vars2Waste)
    NumCaseG=length(input$Vars2Cas)
    if(NumWasteG+NumCaseG==0){
      return()
    }
    plots=Make_Plots2()
    plots[[1]]=plots[[1]] + Header_theme(ConfigOption())
    #Add Site labels
    #add x axis labels
    if(NumCaseG!=0){
      M=NumCaseG
      Te=NumCaseG+1
      if(NumWasteG==0){
        T=NumCaseG
      }
    }else{
      M=1
      Te=1
    }

    plots[[M]]=plots[[M]] +
      theme(axis.text.x = element_text(size = ConfigOption()$XAxisLabSiz, colour = "black"),
            plot.margin = unit(c(.5,0,1,0), "cm"))
    plots[[Te]]=plots[[Te]] + Second_theme(ConfigOption())
    UnifiedPlotsCases=ggarrange(plotlist=plots[1:NumCaseG],ncol =1,common.legend = TRUE,legend = "right",align="v")
    if(NumWasteG==0){
      return(UnifiedPlotsCases)
    }
    UnifiedPlotsWaste=ggarrange(plotlist=plots[(NumCaseG+1):(NumWasteG+NumCaseG)],ncol =1,common.legend = TRUE,legend = "right",align="v")
    if(NumCaseG==0){
      return(UnifiedPlotsWaste)
    }
    UnifiedPlots=ggarrange(plotlist=list(UnifiedPlotsCases,UnifiedPlotsWaste),heights=c(NumCaseG,NumWasteG),ncol=1,legend = "right",align="v")
    FinPlot=annotate_figure( left = text_grob("(GC/L)", rot = 90,size=ConfigOption()$GenFontSiz),UnifiedPlots)
    return(FinPlot)
  })