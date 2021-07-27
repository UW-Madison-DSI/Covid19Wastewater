
RollPerPos = function(RollingDF,CaseName,TestName,Facet=NA,n=7){
  TDF <- RollingDF%>%
    mutate(Facet=!!sym(Facet),
           Case=!!sym(CaseName),
           Test=!!sym(TestName))
  FaucetOptions=unique(TDF$Facet)
  FulldayRange=expand.grid(seq.Date(min(TDF$Date),max(TDF$Date), by = "day"),FaucetOptions)%>%
    rename(Date=Var1,Facet=Var2)
  
  FullDataFM <- full_join(TDF,FulldayRange,by=c("Date","Facet"))%>%
    arrange(Facet,Date)%>%
    group_by(Facet)%>%
    mutate(Per_pos=RollPerPosHelperFunc(Case,Test,n=n),
           Case=RollAvgHelperFunc(Case,N=n,Method="AR"),
           Test=RollAvgHelperFunc(Test,N=n,Method="AR")
           )%>%
    ungroup()
  FullDataFM[[CaseName]]=FullDataFM$Case
  FullDataFM[[TestName]]=FullDataFM$Test
  
  FullDataFM <- FullDataFM%>%
    select(-Facet,-Test,-Case)
  return(FullDataFM)
}


RollPerPosHelperFunc = function(vectorCases,vectorTests,n=7){
  CurrNumCase = rep(NA,n)
  CurrNumTests = rep(NA,n)
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    if(!is.na(vectorCases[i])&&!is.na(vectorTests[i])){
      CurrNumCase[(i-1)%%n+1]=vectorCases[i]
      CurrNumTests[(i-1)%%n+1]=vectorTests[i]
    }else{
      CurrNumCase[(i-1)%%n+1]=NA
      CurrNumTests[(i-1)%%n+1]=NA
    }
    SlideMeanVec[i]=100*sum(CurrNumCase,na.rm=T)/sum(CurrNumTests,na.rm=T)
  }
  return(SlideMeanVec)
}

RollAvg = function(RollingDF,FacetName="Site",n=21,method="Geo",var=c("ReportedCases","EpisodeCases","CollectedCases","ConfirmedCases")){
  TDF=RollingDF%>%
    mutate(Facet=!!sym(FacetName))
  FaucetOptions=unique(TDF$Facet)
  FulldayRange=expand.grid(seq.Date(min(TDF$Date),max(TDF$Date), by = "day"),FaucetOptions)%>%
    rename(Date=Var1,Facet=Var2)
  
  FullDataFM=full_join(TDF,FulldayRange,by=c("Date","Facet"))%>%
    arrange(Facet,Date)%>%
    group_by(Facet)%>%
    mutate(across(any_of(var),RollAvgHelperFunc,N=n,Method=method))%>%
    ungroup()
  FullDataFM[[FacetName]]=FullDataFM$Facet
  FullDataFM=FullDataFM%>%
    select(-Facet)
  return(FullDataFM)
}

RollAvgHelperFunc = function(vectorCases,N=14,Method="Geo"){
  CurrNumCase = rep(NA,N)
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    CurrNumCase[(i-1)%%N+1]=vectorCases[i]
    if(Method == "Geo"){
      slider = exp(mean(log(CurrNumCase),na.rm=T))
    } else{
      slider = mean(CurrNumCase,na.rm=T)
    }
    SlideMeanVec[i]=slider
    #exp(mean(log(CurrNumCase),na.rm=T))
    #mean(CurrNumCase,na.rm=T)
  }
  return(SlideMeanVec)
}

WeekendGen = function(DateVec){
  #Generating the weekends starts and end dates
  #TO DO:Capture the weekend if the data intersects with it
  DateRangeDF=data.frame(Date=seq(min(DateVec), max(DateVec), "days"))%>%
    mutate(Days=weekdays(Date))%>%
    filter(Days %in% c("Sunday","Monday"))
  if (DateRangeDF$Days[1]=="Monday"){
    DateRangeDF=DateRangeDF[-1,]}
  if (tail(DateRangeDF$Days, n=1)=="Sunday"){
    DateRangeDF=head(DateRangeDF, -1)}
  MRan=filter(DateRangeDF,Days=="Sunday")%>%
    rename(Left=Date)
  SRan=filter(DateRangeDF,Days=="Monday")%>%
    rename(Right=Date)
  DateRangeCDF=cbind(MRan,SRan)%>%
    select(Left,Right)
  return(DateRangeCDF)
}


LoessGenerater <- function(Data,weights,Span,min,max){
  if(weights=="Constant"){
    WeightList=rep(1,dim(Data)[1])
  } else if(weights=="N/NSE"){
    WeightList=Data$Indy/Data$Dep
  }
  
  
  loessModel=loessFit(y=Data$Indy,
                      x=Data$Date,
                      weights=WeightList,
                      span=Span,
                      min.weight=min,
                      max.weight=max,
                      iterations=10)
  return(loessModel$fitted)
}



LoessSmoothPlot <- function(SiteS="Madison",
                            Data=LIMSFullDF,
                            Independent="N1",Dependent="N1Error",
                            weights=c("Constant","N/NSE"),
                            Span=.3,min=1e-5,max=1e5,
                            BoxWidth=.5,
                            HasNo=F, Title=T,FullLim=F,SiteLab=T,NoLabel=F){
  
  LimitLIMSDF=Data%>%
    filter(Site==SiteS)%>%
    mutate(Indy=!!sym(Independent),
           Dep=!!sym(Dependent))%>%
    filter(!is.na(Dep),
           !is.na(Indy),
           Dep>0,
           Indy>0)
  
  
  LimitLIMSDF$Pred1 <- LoessGenerater(LimitLIMSDF,weights=weights[[1]],Span=Span,min = min,max = max)
  LimitLIMSDF$PredSE <- LoessGenerater(LimitLIMSDF,weights=weights[[2]],Span=Span,min = min,max = max)
  LoessPlot=LimitLIMSDF%>%
    mutate(ErrorMin=Indy-Dep,ErrorMax=Indy+Dep)%>%
    mutate(ErrorMin=ifelse(ErrorMin>0,ErrorMin,0))%>%
    ggplot()+
    aes(x=Date)+
    geom_rect(aes(ymin=ErrorMin,ymax=ErrorMax,xmin=Date-BoxWidth,xmax=Date+BoxWidth),color="black",alpha=.25)+
    geom_rect(aes(ymin=Indy,ymax=Indy,xmin=Date-BoxWidth,xmax=Date+BoxWidth),color="black",alpha=.25)+
    geom_line(aes(y=Pred1,color="1"),size=1.25)+
    geom_line(aes(y=PredSE,color=paste0(Independent,"/",Dependent)),size=1.25)
  if(SiteLab){
    LoessPlot=LoessPlot+facet_wrap(~Site)
  }
  LoessPlot=LoessPlot+
    labs(title="", y=paste0(Independent," (GC/L)"),
         color="Weights Used")+
    #Header_theme(ConfigOption)+
    theme(axis.text = element_text(size = ConfigOption$XAxisLabSiz, colour = "black"),
          axis.title = element_text(size = ConfigOption$GenFontSiz, colour = "black"),
          plot.margin = unit(c(0,0,0,0), "cm"))+ 
    theme(legend.position = "bottom")
  
  #Formatting code
  
  PlaceHolder=""
  if(HasNo){
    PlaceHolder="no"
  }
  
  if(Title){
    LoessPlot=LoessPlot+
      ggtitle(paste0("Weighted loess smoothing with ",PlaceHolder," truncation 
                     loessFit(",Independent, " ~ Date, span=",Span,")"))
  }
  if(FullLim){
    XVar=LIMSFullDF$Date
    YVar=pull(LIMSFullDF,Independent)
    XLim <- c(min(XVar),max(XVar))
    YLim <- c(min(YVar[YVar!=0]),max(YVar))
    LoessPlot <- LoessPlot+
      scale_x_date(limits = XLim)+
      scale_y_log10(limits = YLim)
  }else{
    LoessPlot <- LoessPlot+
      scale_y_log10()
  }
  if(NoLabel){
    LoessPlot <- LoessPlot + 
      labs(title="",x="",y="")
    # theme(axis.title.x=element_blank(),
    #     axis.title.y=element_blank())
  }
  return(LoessPlot)
}