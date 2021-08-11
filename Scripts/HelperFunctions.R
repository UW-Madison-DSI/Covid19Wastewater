
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

DataPrep <- function(DF,SiteS=NA,keep=c()){
  if(!is.na(SiteS)){
    FilteredVec1 <- filter(DF,Site==SiteS)
  }else{
    FilteredVec1 <- DF
  }
  
  FullDayDF <- data.frame(Date=seq(min(FilteredVec1$Date),
                                   max(FilteredVec1$Date),1))
  ReadyDF <- full_join(FullDayDF,FilteredVec1, by = c("Date"))%>%
    fill(one_of(keep), .direction = "down")%>%
    mutate(Site=SiteS)%>%
    select(Date,one_of(keep),Site,)
  return(ReadyDF)
}



DFSmoothingFNC <- function(CaseDF,
                           SiteS,
                           PreRoll=FALSE,
                           Weights=dgamma(1:21,scale =5.028338,shape =2.332779)){
  #5.028338
  #2.332779
  
  FullDayDF <- DataPrep(CaseDF,"Cases",SiteS=SiteS)
  
  if(PreRoll){
    FullDayDF <- FullDayDF%>%
      group_by(Site)%>%
      mutate(Cases2 = c(rep(NA,6),
                        rollapply(Cases,width=7,
                                  FUN=mean,
                                  na.rm = TRUE)))
  }else{
    FullDayDF <- FullDayDF%>%
      mutate(Cases2=Cases)
  }
  FullDayDF <- FullDayDF%>%
    group_by(Site)%>%
    mutate(Cases2 = c(rep(NA,20),
                      rollapply(Cases2,width=21,FUN=weighted.mean,
                                w=Weights,
                                na.rm = TRUE)))
  return(FullDayDF)
}

DFLoessFNC <- function(N1DF,SiteS,span=.125){
  
  ReadyDF <- DataPrep(N1DF,keep=c("N1"),SiteS=SiteS)
  
  loessFit(y=log(ReadyDF$N1),
           x=ReadyDF$Date,
           span=span,
           min.weight=0,
           max.weight=1e5,
           iterations=20)$fitted
}



TSPloting <- function(PlotingTS,SourceDF,DepName,IndName,FullPlot=TRUE,SubTitle=NA,FirstDif=FALSE){
  if(FirstDif){
    PlotingTS[[2]] <- diff(PlotingTS[[2]])
    PlotingTS[[1]] <- diff(PlotingTS[[1]])
    Lab <- paste("First Diffrence of", DepName, "and", IndName)
  }else{
    Lab <- paste("Visual relationship of", DepName, "and", IndName)
  }
  plot.new()
  par(mar = c(5, 4, 4, 4) + 0.3)           
  
  if(FullPlot){
    plot.ts(rollmean(exp(PlotingTS[[4]]), 7,align="right",fill = NA),
            col = "steelblue3", 
            axes = FALSE, 
            xlab = "", 
            ylab = "",
            xaxt = "n",
            log="y")
    
    axis(side = 2, col.axis="blue")
    
    par(new = TRUE)  
    plot.ts(rollmean(PlotingTS[[3]], 7,align="right",fill = NA),
            col = "hotpink",        
            axes = FALSE, xlab = "", ylab = "",xaxt = "n")
    
    par(new = TRUE)  
    plot.ts(exp(PlotingTS[[2]]),
            col = "blue",        
            axes = FALSE, 
            xlab = "", 
            ylab = "",
            log = 'y',
            xaxt = "n")
  }else{
    plot.ts(exp(PlotingTS[[2]]), 
            col = "blue", 
            axes = FALSE,
            main=Lab,
            ylab="",
            sub=SubTitle,
            log = 'y',
            xaxt = "n")
    axis(side = 2, col.axis="blue")
  }
  
  par(new = TRUE)                           
  plot.ts(PlotingTS[[1]], col = "red",        
          axes = FALSE, xlab = "", ylab = "",xaxt = "n")
  
  axis(side = 4, at = pretty(range(PlotingTS[[1]])),col.axis="red")
  mtext("Cases", side = 4, line = 3, col = "red")
  mtext("N1 (GC/L)", side = 2, line = 3, col = "blue")
  if(FullPlot){
    legendNames <- c("SLD Cases","7 MA Cases","Loess Smoothing","7 MA N1")
    legendColors <- c("red","hotpink","blue", "steelblue3")
  }else{
    legendNames<- c("SLD Cases","Loess Smoothing")
    legendColors<- c("red","blue")
  }
  legend("topright", legend=legendNames, col=legendColors, lty=1, cex=.75)
  axis(1,
       pretty(SourceDF$Date),labels =format(pretty(SourceDF$Date), "%Y-%m-%d"))
  #pretty(format(SourceDF$Date, "%Y-%m-%d")))
}

SLDGraphics <- function(SiteS,DepTSVec,IndTSVec,DepName,IndName,efficient=FALSE){
  print("Visual Relationship")
  TSPloting(list(DepTSVec,IndTSVec),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS)
  TSPloting(list(DepTSVec,IndTSVec),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS,FirstDif=TRUE)
  
  CCFVec <- ccf(IndTSVec,DepTSVec,
                main=paste("CC between",IndName,"and", DepName),
                sub=SiteS)
  OffSet <- which(CCFVec[[1]]==max(CCFVec[[1]]))-21 #Best offset of Straight ccf
  
  
  preWhiteFit <- auto.arima(IndTSVec, seasonal=FALSE,
                            stepwise=FALSE, approximation=FALSE) #underlying arima trend of Ind
  
  IndResid <- IndTSVec - fitted(Arima(IndTSVec, model = preWhiteFit))
  DepResid <- DepTSVec - fitted(Arima(DepTSVec, model = preWhiteFit))
  CCFVecPre <- ccf(IndResid, DepResid,lag.max=22,
                   main=paste("prewhiten CC between",IndName,"and", DepName),
                   sub=SiteS) #CC removing Arima relationship of Ind
  
  OffSetWhit <- which(CCFVecPre[[1]]==max(CCFVecPre[[1]]))-21#Best offset of PreWhite ccf
  
  if(max(CCFVecPre[[1]])<.15){ #If no prewhite corr is significant then use straight ccf
    print("no signifigent prewhitened relationship")
    OffSetWhit <- OffSet
  }
  
  print(paste("using offset of", OffSetWhit))
  
  TSUnionDF <- ts.intersect(DepTSVec,
                            OGVec = stats::lag(IndTSVec,OffSetWhit))
  
  print("Visual Relationship with offset")
  TSPloting(list(TSUnionDF[,1],TSUnionDF[,2]),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS)
  TSPloting(list(TSUnionDF[,1],TSUnionDF[,2]),MergedDF,DepName,IndName,
            FullPlot=FALSE,SubTitle=SiteS,FirstDif=TRUE)
  
  
  OLM <- lm(TSUnionDF[,1]~TSUnionDF[,2])
  print("Ordinary LM")
  print(summary(OLM))
  ggtsdisplay(residuals(OLM))
  FM <- auto.arima(TSUnionDF[,1],xreg=TSUnionDF[,2])
  #FM <- cochrane.orcutt(OLM,max.iter=1000)
  print("LM with Arima residuals")
  ggtsdisplay(residuals(FM))
  print(summary(FM))
  return(FM)
}



TSPloting2 <- function(PlotingTS,SourceDF,SubTitle,SLD=TRUE,span=.125){
  if(SLD){
    Shade <- 0.5
    Thickness <- 1
  }else{
    Shade <-1
    Thickness <- 2
  }
  RangeCases <- range(PlotingTS[[1]],na.rm=TRUE)
  RangeN1 <- range(exp(PlotingTS[[2]]),na.rm=TRUE)
  plot.new()
  par(mar = c(8, 4, 4, 4) + 0.1)
  plot(1, type="n", xlab=SubTitle, ylab="", axes = FALSE,
       xlim=range(SourceDF$Date), ylim=RangeN1,log="y")
  rect(SourceDF$Date-.5,
       SourceDF$N1-SourceDF$N1Error,
       SourceDF$Date+.5,
       SourceDF$N1+SourceDF$N1Error,
       col  = rgb(0, 0, 1, alpha=0.25),
       border  = NA,
       xlab = "",
       ylab = "",
       xaxt = "n")
  par(new = TRUE)
  barplot(as.numeric(PlotingTS[[3]]),
          col  = rgb(1, 0, 0, alpha=0.1),
          xlab = "",
          ylab = "",
          xaxt = "n",
          border = NA,
          ylim = RangeCases,
          axes = FALSE)
  par(new = TRUE)
  
  plot.ts(rollmean(PlotingTS[[3]], 7,align="right",fill = NA),
          col = rgb(1, 0, 0, alpha=Shade),
          ylim = RangeCases,
          axes = FALSE,
          lwd=Thickness,
          xlab = "",
          ylab = "",
          xaxt = "n")
  axis(4, pretty(RangeCases),col.axis = "red",cex.axis=.75)
  
  RangeDates <- range(SourceDF$Date)
  ticks <- seq(RangeDates[1],RangeDates[2], by = "month")
  axis(1,
       ticks,
       labels = FALSE,
       cex.axis=.5)
  text(cex=.75, x=ticks-15, y=-115, format(ticks,"%b %Y"),
       xpd=TRUE, srt=30)
  
  par(new = TRUE)
  plot.ts(exp(PlotingTS[[2]]),
          col = "blue",
          ylim = RangeN1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          log="y",
          lwd=2,
          xaxt = "n")
  axis(2,col.axis = "blue",cex.axis=.75)
  
  legendNames <- c("7 day MA Cases",paste("loess smoothing with span=",span))
  legendColors <- c(rgb(1, 0, 0, alpha=Shade),"blue")
  if(SLD){
    legendNames <- c("Shedding lag distribution",legendNames)
    legendColors <- c("red",legendColors)
    
    par(new = TRUE)
    plot.ts(PlotingTS[[1]],
            ylim = RangeCases,
            col = "red",
            lwd=2,
            axes = FALSE,
            xlab = "", ylab = "",xaxt = "n")
  }
  mtext("Cases", side = 4, line = 2, col = "red",cex=.75)
  mtext("N1 (GC/L)", side = 2, line = 2, col = "blue",cex=.75)
  
  
  legend("topright", legend=legendNames, col=legendColors, lty=1, cex=.5)
  
}



