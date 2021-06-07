Buildplot_gen = function(vari,MainDF,Standards,Loc=NA,ColorType=NA,spanN=NA,
                         LineDF=NA, MeanDF=NA ,DateLimits=NA,WeekDF=NA,
                         AxisPos="top",log_scale=F,RMOutliers=F, 
                         IgnoreLog = c("Pct_BCoV"), Xfreq="24 days",
                         LineColor="black",YLabel=NA,norm=NA, boxingDF=NA,
                         Colplot=F,nrow=1,scalesF="fixed",Start=NA,End=NA){
  Leb=vari
  workDataFrameMain=MainDF%>%
    mutate(var=!!sym(vari))
  if(!is.na(norm)){
    workDataFrameMain=workDataFrameMain%>%
      mutate(var=var/!!sym(norm))
    Leb=paste(vari,"per 100 thousand people")
  }
  set.seed(Standards$myseed)
  GPlot=ggplot()
  if(Colplot&!is.na(MainDF)){
    GPlot=ColGen(GPlot,workDataFrameMain,Standards,ColorType)
  }
  else if(!is.na(MainDF)){
  GPlot=PointGen(GPlot,workDataFrameMain,Standards,ColorType)
  }
  if(is.data.frame(MeanDF)){
    workDataFrameMean=MeanDF%>%
      mutate(var=!!sym(vari))
    if(!is.na(norm)){
      workDataFrameMean=workDataFrameMean%>%
        mutate(var=var/!!sym(norm))
    }
    GPlot=PointGen(GPlot,workDataFrameMean,Standards,ColorType,Size=2.5,Bold=TRUE)
  }
  GPlot=GPlot+ylab(Leb)
  if(!is.na(spanN)){
    GPlot=GPlot+geom_smooth(data=workDataFrameMain,aes(y=var,x=Date),span=spanN,na.rm=TRUE)
  }
  if(is.data.frame(LineDF)){
    workDataFrameLine=LineDF%>%
      mutate(var=!!sym(vari))
    if(!is.na(norm)){
      workDataFrameLine=workDataFrameLine%>%
        mutate(var=var/!!sym(norm))
    }
    GPlot=GPlot+geom_line(data=workDataFrameLine,aes(y=var,x=Date),color=LineColor,size=1,na.rm=TRUE)
  }
  
  if(!is.na(boxingDF)){
    GPlot=GPlot+geom_rect(aes(ymin=lower,ymax=Upper,xmin=Date-.3,xmax=Date+.3),fill="red",alpha=.4,data=meanOfN1)
  }
  
  if(!is.na(Start)&!is.na(End)){
    GPlot=GPlot+geom_vline(xintercept = Start,linetype="dashed")
    GPlot=GPlot+geom_vline(xintercept = End,linetype="dashed")
  }
  

  rec_min=-Inf
  if(log_scale&&!(vari %in% IgnoreLog)){
    GPlot = GPlot + scale_y_log10()
    rec_min=0
  }
  if(is.data.frame(WeekDF)){
    GPlot=GPlot+geom_rect(data=WeekDF, 
                          aes(xmin=Left, xmax=Right, ymin=rec_min, ymax=Inf),
                          fill='pink', alpha=Standards$alphaWeek,na.rm=TRUE)
  }
  VarVec=workDataFrameMain$var
  ValLimMin=min(VarVec)
  if (RMOutliers){
    if(log_scale){
      ValLimMax=quantile(VarVec,.995,na.rm=TRUE)[[1]]
    }
    else{
      ValLimMax=quantile(VarVec,.975,na.rm=TRUE)[[1]]
    }
  }
  else{
    ValLimMax=max(VarVec)
  }
  if(!is.na(YLabel)){
    GPlot=GPlot+ylab(YLabel)
  }
  ValLimits=c(ValLimMin,ValLimMax)
  if(is.na(scalesF)){
    GPlot=GPlot+coord_cartesian(ylim=ValLimits)
  }
  GPlot=GPlot+coord_cartesian(xlim=DateLimits)
  GPlot=GPlot+scale_x_date(position=AxisPos,date_breaks=Xfreq,date_labels="%b %d")
  GPlot=GPlot+facet_wrap(as.formula(paste("~", Loc)), nrow = nrow,scales=scalesF)+Middle_theme(Standards)
  return(GPlot)
}

PointGen = function(Plot,DF,Standards,ColorType,Size=1,Bold=F){

  if(Bold){
    shape=23
    Alpha=1
  }else{
    shape=21
    Alpha=Standards$alphaPoint
  }
  RPlot=Plot
  if(!is.na(ColorType)){
    RPlot=RPlot+geom_jitter(data=DF,
                            aes(y=var,x=Date,
                                fill = !!sym(ColorType),
                                color= !!sym(ColorType)),
                            shape = shape,
                            alpha=Alpha,
                            height=0,width=.1,
                            size=Size*Standards$PointSize,na.rm=TRUE)
    
  }else{
    RPlot=RPlot+geom_jitter(data=DF,aes(y=var,x=Date),
                            shape = shape,
                            fill="Black",
                            alpha=Alpha,height=0,width=.1,
                            size=Size*Standards$PointSize,na.rm=TRUE)
  }
  if(Bold){
    #slightly jank
    RPlot=RPlot+scale_color_manual(values=rep("Black",3))
  }
  return(RPlot)
}

ColGen = function(Plot,DF,Standards,ColorType,Size=1,width=.4){
  RPlot=Plot
  if(!is.na(ColorType)){
    RPlot=RPlot+geom_rect(data=DF,aes(ymin=0,ymax=var,xmin=Date-width,
                            xmax=Date+width, fill=!!sym(ColorType)),
                            alpha=Standards$alphaPoint,
                            size=Size*Standards$PointSize,na.rm=TRUE)+ 
      scale_fill_manual(values=c("light blue","gray"))
    
  }else{
    RPlot=RPlot+geom_rect(data=DF,aes(ymin=0,ymax=var,xmin=Date-width,
                                      xmax=Date+width,),
                          fill= "gray", alpha=Standards$alphaPoint, 
                          size=Size*Standards$PointSize,na.rm=TRUE)
  }
  return(RPlot)
}



Middle_theme = function(ConfigOption){
  theme_grey() %+replace%    #Theme for the plots in the middle
    theme(
      plot.margin = unit(c(.5,0,0,0), "cm"),
      text = element_text(size=ConfigOption$GenFontSiz),
      axis.title.x = element_blank(),
      strip.text.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(size = ConfigOption$YAxisLabSiz, colour = "black"))
}
Header_theme = function(ConfigOption){
  Middle_theme(ConfigOption) %+replace%    #Theme for the top plots
    theme(plot.margin = unit(c(.5,0,0,0), "cm"),
          strip.text.x = element_text(size = ConfigOption$GenFontSiz, colour = "black"))
}
Second_theme = function(ConfigOption){
  Middle_theme(ConfigOption) %+replace%    #Theme for the plots underneither the first plots
    theme(plot.margin = unit(c(1,0,0,0), "cm"),
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
    ggplot(aes(x=`time lag`)) + geom_tile(aes(y=location, fill= Correlation),na.rm=TRUE) + 
    geom_tile(aes(y=SiteBest),fill=NA,color="white",size=1,show.legend = FALSE,na.rm=TRUE)+
    theme(axis.text.x = element_text(angle = 90))+ scale_fill_continuous(type = "viridis",limits=c(-1,1))+
    geom_text(aes(y=SiteBest,label = round(Correlation, 2)),na.rm=TRUE)
  return(plotedGraph)
}

library(lubridate)
BoxPlotProduction = function(wastewaterDF,Time,concentration,Loc,BinSiz=7,DateLimits=NA){
  BoxGraphic=wastewaterDF%>%
    mutate(Date=!!sym(Time),N1=!!sym(concentration),Site=!!sym(Loc))%>%
    mutate(arbataryBin = as.Date(BinSiz*((as.numeric(Date) %/% BinSiz) - (as.numeric(min(Date)) %/% BinSiz))+as.numeric(min(Date)),origin = as.Date("1970-01-01")))%>%
    mutate(logN1=log(N1))%>%
    group_by(arbataryBin,Site)%>%
    mutate(meanC=exp(mean(logN1,na.rm=TRUE)))%>%
    ggplot()+
    geom_boxplot(aes(y=N1,x=arbataryBin,group=arbataryBin),fill="light blue",na.rm=TRUE)+
    geom_point(aes(y=meanC,x=arbataryBin),shape = 4,color="red")+coord_cartesian(xlim=DateLimits)+scale_y_log10()+
    facet_wrap(~Site,nrow = 1)+scale_x_date(date_breaks="21 days",date_labels="%b %d")
  return(BoxGraphic)
}

