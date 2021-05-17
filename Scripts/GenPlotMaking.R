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