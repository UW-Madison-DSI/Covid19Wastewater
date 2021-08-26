
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

RollAvg = function(RollingDF,FacetName="Site",n=21,method="Arth",var=c("ReportedCases","EpisodeCases","CollectedCases","ConfirmedCases")){
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

DFLoessFNC <- function(N1DF,Var="N1",SiteS,span=.125){
  
  ReadyDF <- DataPrep(N1DF,keep=c(Var),SiteS=SiteS)
  
  loessFit(y=log(ReadyDF[,Var]),
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
    plot.ts(PlotingTS[[2]], 
            col = "blue", 
            axes = FALSE,
            main=Lab,
            ylab="",
            sub=SubTitle,
            log = '',
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
  CCFVecPre <- ccf(IndResid, DepResid,lag.max=10,
                   main=paste("prewhiten CC between",IndName,"and", DepName),
                   sub=SiteS) #CC removing Arima relationship of Ind
  
  OffSetWhit <- which(CCFVecPre[[1]]==max(CCFVecPre[[1]]))-21#Best offset of PreWhite ccf
  
  if(TRUE){#max(CCFVecPre[[1]])<.15){ #If no prewhite corr is significant then use straight ccf
    print("Ignoring preWhite process")
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



TSPloting2 <- function(PlotingTS,SourceDF,SubTitle,
                       SLD=TRUE,span=.125){
  if(SLD){
    Shade <- 0.5
    Thickness <- 1
  }else{
    Shade <-1
    Thickness <- 2
  }
  N1Axis=""
  CasesAxis=""
  RangeCases <- range(PlotingTS[[1]],na.rm=TRUE)
  RangeN1 <- range(PlotingTS[[2]],na.rm=TRUE)
  MaxN1 <- max(SourceDF$N1,na.rm=TRUE)
  MinN1 <- min(SourceDF$N1,na.rm=TRUE)
  MaxCases <- max(PlotingTS[[3]],na.rm=TRUE)
  N1Ratio <- log(MaxN1)/log(RangeN1[2])
  CasesRatio <- log(MaxCases)/(log(RangeCases[2]))
  Displace <- max(N1Ratio,CasesRatio)
  print(paste(RangeCases[[1]]))
  
  #RangeCases[2] <- RangeCases[2]*exp(Displace+1.5)
  #RangeN1[2] <- RangeN1[2]*exp(Displace)
  
  RangeCases[1] <- 0 #RangeCases[1]-4
  
  plot.new()
  par(mar = c(8, 4, 4, 4) + 0.1)
  plot(1, type="n", xlab=SubTitle, ylab="", axes = FALSE,
       xlim=range(SourceDF$Date), ylim=RangeN1,log=N1Axis)
  rect(SourceDF$Date-.5,
       SourceDF$N1-SourceDF$N1Error,
       SourceDF$Date+.5,
       SourceDF$N1+SourceDF$N1Error,
       col  = rgb(0, 0, 1, alpha=0.25),
       border  = NA,
       ylim = RangeN1,
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
          log=CasesAxis,
          ylim = RangeCases,
          axes = FALSE)
  par(new = TRUE)
  
  plot.ts(rollmean(PlotingTS[[3]], 7,align="right",fill = NA),
          col = rgb(1, 0, 0, alpha=Shade),
          ylim = RangeCases,
          axes = FALSE,
          lwd=Thickness,
          log=CasesAxis,
          xlab = "",
          ylab = "",
          xaxt = "n")
  axis(4,col.axis = "red",cex.axis=.75)
  
  RangeDates <- range(SourceDF$Date)
  ticks <- seq(RangeDates[1],RangeDates[2], by = "month")
  axis(1,
       ticks,
       labels = FALSE,
       cex.axis=.5)
  text(cex=.75, x=ticks-2, y=-30, format(ticks,"%b %Y"),
       xpd=TRUE, srt=30)
  
  par(new = TRUE)
  plot.ts(PlotingTS[[2]],
          col = "blue",
          ylim = RangeN1,
          axes = FALSE,
          xlab = "",
          ylab = "",
          log=N1Axis,
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
            log=CasesAxis,
            xlab = "", ylab = "",xaxt = "n")
  }
  mtext("Cases", side = 4, line = 2, col = "red",cex=.75)
  mtext("N1 (GC/L)", side = 2, line = 2, col = "blue",cex=.75)
  
  
  legend("topright", legend=legendNames, col=legendColors, lty=1, cex=.5)
  
}

CheckFunction <- function(StartDate=0:7,DaySmoothing=c(1,7,14),Lag=-2:2,
                          Show2=FALSE,Mat=FALSE,Ret="R2",CasesUsed="Cases4",
                          DateStart=mdy("11/1/2020")){
  SDL <- length(StartDate)
  DSL <- length(DaySmoothing)
  LL <- length(Lag)
  #array(numeric(),c(SDL*DSL*LL))
  Matrix=vector(mode="numeric", length=SDL*DSL*LL)
  for (j in 1:DSL){
    for (k in 1:LL){
      for (i in 1:SDL){
        Matrix[j*SDL*LL+k*SDL+i-SDL*LL-SDL]=PlotingOptions(StartDate[i],
                                        DaySmoothing[j],Lag[k],
                                        Ret=Ret,CasesUsed=CasesUsed,
                                        DateStart=DateStart)
      }
    }
  }
  if(Ret=="PVal"){
    Loc <- which.min(Matrix)
    Target=min(Matrix)
  }else{
    Loc <- which.max(Matrix)
    Target=max(Matrix)
  }
  iL <- StartDate[((Loc-1)%%SDL)+1]
  jL <- DaySmoothing[(Loc-1)%/%(SDL*LL)+1]
  kL <- Lag[((Loc-1)%%(SDL*LL)%/%SDL)+1]

  stopifnot(Target==PlotingOptions(iL,jL,kL,
                                    Ret=Ret,CasesUsed=CasesUsed,
                                    DateStart=DateStart))

  BestLM <- PlotingOptions(iL,jL,kL,Show=Show2,
                           Ret="LM",CasesUsed=CasesUsed,
                           DateStart=DateStart)
  
  SlopeL <- signif(BestLM[[1]][1],3)
  if(Show2){
    print(paste("Best relationship at",iL,jL,kL,"with score of",
                max(Matrix),"with F factor of",SlopeL))
  }
  if(Mat){
    return(Matrix)
  }
  return(BestLM)
}

ReplacementFilter <- function(n,Main,Rep){
  Noise <- abs((Main-Rep)/Rep)
  Noise[is.na(Noise)] <- FALSE
  NoiseFilter <- sort(Noise,TRUE)[n+1]
  #length(logN1VecFiltA[Noise>=NoiseFilter])
  Ret <- Main
  Ret[Noise>NoiseFilter] <- Rep[Noise>NoiseFilter]
  return(Ret)
}


PlotingOptions <- function(StartDate,DaySmoothing,Lag,
                           Show=FALSE,Ret="LM",CasesUsed="Cases4",
                           DateStart=mdy("11/1/2020")){
  MadData <- MergedDF%>%
    filter(Date>DateStart)%>%
    mutate(MovedCases = data.table::shift(Cases2,Lag),
           Cases3 = data.table::shift(Cases,Lag),
           Week=as.numeric(Date+StartDate)%/%DaySmoothing)%>%
    group_by(Week)%>%
    summarise(NM=median(N1,na.rm=TRUE),
              Cases4=mean(Cases3,na.rm = TRUE),
              CasesM=mean(MovedCases,na.rm = TRUE))%>%
    mutate(Cases4=c(NA,NA,rollapply(Cases4,width=3,FUN=weighted.mean,
                                    w=WeightVec,
                                    na.rm = TRUE)))%>%
    mutate(CasesMain=!!sym(CasesUsed))
  
  LMod <- lm(CasesMain~NM-1,data=MadData)
  COR <- signif(cor(MadData$CasesMain,MadData$NM,use="pairwise.complete.obs"),3)
  R2 <- signif(summary(LMod)[[8]],3)
  PVal <- signif(summary(LMod)$coefficients[,2],3)

  if(Show){
    Slope=LMod[[1]][1]
    
    DatePlot <- MadData%>%
      ggplot()+
      aes(x=Week)+
      geom_line(aes(y=NM*Slope,color="N1Binned"))+
      geom_line(aes(y=CasesMain,color="SLDBinned"))
    
    CompPlot <- MadData%>%
      ggplot()+
      aes(x=CasesMain,y=NM)+
      geom_point()+
      geom_abline(aes(color="Line of best first",slope=1/Slope,intercept=0))+
      labs(x="7 Day binning of SLD Cases",
           y="7 day binning of N1")+
      annotate("text", x=250, y=4e4, label= paste("R^2:", R2))+
      annotate("text", x=250, y=1e5, label= paste("Cor:", COR))
    
    print(CompPlot)
    print(DatePlot)
  }
  if(Ret=="LM"){
    return(LMod)
  }else if(Ret=="COR"){
    return(COR)
  }else if(Ret=="R2"){
    return(R2)
  }else if(Ret=="PVal"){
    return(PVal)
  }
  
}#StartDate,DaySmoothing,Lag,COR,R2


HeatMapCor <- function(DaySmoothing,Show3=TRUE,CasesUsed="Cases4",
                       DateStart=mdy("11/1/2020")){
  ColorLegend <- brewer.pal(n = 3, name = "YlOrRd")
  Color <- brewer.pal(n = 8, name = "YlOrRd")
  R2Mat <- matrix(CheckFunction(DaySmoothing=DaySmoothing,Show2=Show3,
                                Mat=TRUE,Ret="R2",CasesUsed=CasesUsed,DateStart=DateStart),
                  nrow=8)
  
  PValMat <- matrix(CheckFunction(DaySmoothing=DaySmoothing,Show2=Show3,
                                  Mat=TRUE,Ret="PVal",CasesUsed=CasesUsed,
                                  DateStart=DateStart),nrow=8)
  
  CorMat <- matrix(CheckFunction(DaySmoothing=DaySmoothing,Show2=Show3,
                                 Mat=TRUE,Ret="COR",CasesUsed=CasesUsed,
                                 DateStart=DateStart),nrow=8)
  
  
  heatmap(R2Mat,Rowv=NA,Colv=NA,col = Color ,main="R2 relationship")
  legend(x="right", legend=c(round(min(R2Mat),1),
                             round(median(R2Mat),2),
                             round(max(R2Mat),2)),fill=ColorLegend)
  
  heatmap(PValMat,Rowv=NA,Colv=NA,col = Color,main="PVal relationship")
  legend(x="right", legend=c(round(min(PValMat),1),
                             round(median(PValMat),1),
                             round(max(PValMat),1)),fill=ColorLegend)
  
  heatmap(CorMat,Rowv=NA,Colv=NA,col = Color,main="Cor relationship")
  legend(x="right", legend=c(round(min(CorMat),1),
                             round(median(CorMat),1),
                             round(max(CorMat),1)),fill=ColorLegend)
}

