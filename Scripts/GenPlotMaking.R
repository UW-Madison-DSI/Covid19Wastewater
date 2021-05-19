Buildplot_gen = function(vari,MainDF=NA,Loc=NA,ColorType=NA,spanN=NA,LineDF=NA,
                         LineVari=NA,MeanDF=NA ,DateLimits=NA,WeekDF=NA,
                         AxisPos="top",Standards=NA,log_scale=F,RMOutliers=F,
                         Xfreq="24 days",LineColor="red"){
  set.seed(Standards$myseed)
  GPlot=ggplot(data=MainDF)
  if(!is.na(ColorType)){
    GPlot=GPlot+geom_jitter(data=MainDF,aes(y=!!sym(vari),x=Date,
                            color=!!sym(ColorType)),alpha=Standards$alphaPoint,
                            height=0,width=.1,size=Standards$PointSize)
  }else{
    GPlot=GPlot+geom_jitter(data=MainDF,aes(y=!!sym(vari),x=Date),
                            alpha=Standards$alphaPoint,height=0,width=.1,
                            size=Standards$PointSize)
  }
  GPlot=GPlot+ylab(vari)
  if(!is.na(spanN)){
    GPlot=GPlot+geom_smooth(aes(y=!!sym(vari),x=Date),span=spanN)
  }
  if(!is.na(LineDF)){
    GPlot=GPlot+geom_line(data=LineDF,aes(y=!!sym(vari),x=Date),size=1)
  }
  else if(!is.na(LineVari)){
    GPlot=GPlot+geom_line(data=MainDF,aes(y=!!sym(LineVari),x=Date),color=LineColor,
                          size=1)
  }
  if(!is.na(MeanDF)){
    GPlot=GPlot+geom_jitter(data=MeanDF,aes(y=!!sym(vari),x=Date,
                          fill=!!sym(ColorType)), shape=23, 
                          size=2.5*Standards$PointSize, height=0, width=.1)
  }
  rec_min=-Inf
  if(log_scale){
    GPlot = GPlot + scale_y_log10()
    rec_min=0
  }
  if(!is.na(WeekDF)){
    GPlot=GPlot+geom_rect(data=WeekDF, 
                          aes(xmin=Left, xmax=Right, ymin=rec_min, ymax=Inf),
                          fill='pink', alpha=Standards$alphaWeek)
  }
  VarVec=pull(MainDF,vari)
  ValLimMin=min(VarVec)
  if (RMOutliers){
    if(log_scale){
      ValLimMax=quantile(VarVec,.995)[[1]]
    }
    else{
      ValLimMax=quantile(VarVec,.975)[[1]]
    }
  }
  else{
    ValLimMax=max(VarVec)
  }
  ValLimits=c(ValLimMin,ValLimMax)
  GPlot=GPlot+coord_cartesian(ylim=ValLimits,xlim=DateLimits)
  GPlot=GPlot+scale_x_date(position=AxisPos,date_breaks=Xfreq,date_labels="%b %d")
  GPlot=GPlot+facet_wrap(as.formula(paste("~", Loc)), nrow = 1)+Middle_theme(Standards)
  return(GPlot)
}

Middle_theme = function(ConfigOption){
  theme_grey() %+replace%    #Theme for the plots in the middle
    theme(
      text = element_text(size=ConfigOption$GenFontSiz),
      axis.title.x = element_blank(),
      strip.text.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = ConfigOption$YAxisLabSiz, colour = "black"))
}
Header_theme = function(ConfigOption){
  theme_grey() %+replace%    #Theme for the top plots
    theme(plot.margin = unit(c(1,0,.5,0), "cm"),
          text = element_text(size=ConfigOption$GenFontSiz),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = ConfigOption$YAxisLabSiz, colour = "black"),
          axis.text.x = element_text(size = ConfigOption$XAxisLabSiz, colour = "black"),
          strip.text.x = element_text(size = ConfigOption$GenFontSiz, colour = "black"))
}
Second_theme = function(ConfigOption){
  Middle_theme(ConfigOption) %+replace%    #Theme for the plots underneither the first plots
    theme(plot.margin = unit(c(1.5,0,0,0), "cm"),
          axis.text.x = element_text(size = ConfigOption$XAxisLabSiz, colour = "black"))
}


SiteLagHeatMap = function(CaseDF,WasteDF,seqminmax,Loc,Dat,Forma){
  depenForm=formula(Forma)[3][[1]]
  indepForm=formula(Forma)[2][[1]]
  CaseFixDF=CaseDF%>%
    mutate(location=!!sym(Loc),Date=!!sym(Dat))
  WasteFixDF=WasteDF%>%
    mutate(location=!!sym(Loc),Date=!!sym(Dat))
  cor.df=NA
  for (i in seqminmax){
    if(i!=min(seqminmax)){
      
      cor.df=cor.df%>%
        select(-location)
    }
    TempCaseDF=CaseFixDF%>%
      mutate(Date=Date+i)
    MMSDDF=full_join(TempCaseDF,WasteFixDF,by=c("location","Date"))%>%
      filter(!is.na(location),!is.na(Date))
    TVec=MMSDDF%>%
      mutate( Val= !!depenForm,depVal=!!indepForm)%>%
      group_by(location)%>%
      summarize("{i}":= cor(x = Val, y = depVal, use = "pairwise.complete.obs"))
    cor.df=cbind(cor.df,TVec)
  }
  mincol=toString(min(seqminmax))
  maxcol=toString(max(seqminmax))
  cor.df=cor.df%>%
    select(-cor.df)%>%
    select(location,!location)%>%
    pivot_longer(mincol:maxcol,names_to="time lag",values_to = "Correlation")%>%
    group_by(location)%>%
    mutate(bestCase=max(Correlation))%>%
    mutate(SiteBest=ifelse(Correlation==bestCase,location,NA))
  cor.df$`time lag`=factor(cor.df$`time lag`, levels=c(mincol:maxcol))
  plotedGraph=cor.df%>%
    ggplot(aes(x=`time lag`)) + geom_tile(aes(y=location, fill= Correlation)) + geom_tile(aes(y=SiteBest),fill=NA,color="white",size=1,show.legend = FALSE)+
    theme(axis.text.x = element_text(angle = 90))+ scale_fill_continuous(type = "viridis",limits=c(-1,1))+
    geom_text(aes(y=SiteBest,label = round(Correlation, 2)))
  return(plotedGraph)
}

BoxPlotProduction = function(wastewaterDF,Time,concentration,Loc,Log10B=T){
  BoxGraphic=wastewaterDF%>%
    mutate(Month=format(!!sym(Time),"%B"),Conc=!!sym(concentration),Site=!!sym(Loc))%>%
    group_by(Month)%>%
    mutate(meanC=mean(Conc))%>%
    ggplot()+geom_boxplot(aes(x=Month,y=Conc),fill="light blue")+geom_point(aes(x=Month,y=meanC),shape = 4)+facet_wrap(~Site,ncol = 1)
  if(Log10B){
    BoxGraphic=BoxGraphic+scale_y_log10()
  }
  return(BoxGraphic)
}

