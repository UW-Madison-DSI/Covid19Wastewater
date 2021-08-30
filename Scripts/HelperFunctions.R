
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





DataPrep <- function(DF=NA,SiteS=NA,keep=c()){
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

LocInput <- function(Mat,Loc,StartDate,DaySmoothing,Lag){
  SDL <- length(StartDate)
  LL <- length(Lag)
  iL <- StartDate[((Loc-1)%%SDL)+1]
  jL <- DaySmoothing[(Loc-1)%/%(SDL*LL)+1]
  kL <- Lag[((Loc-1)%%(SDL*LL)%/%SDL)+1]
  return(c(iL,jL,kL))
}


CheckFunction <- function(DF,StartDate=0:7,DaySmoothing=c(1,7,14),Lag=-2:2,
                          Show2=FALSE,Mat=FALSE,Ret="R2",CasesUsed="Cases4",
                          DateStart=mdy("11/1/2020"),Pop=FALSE){
  SDL <- length(StartDate)
  DSL <- length(DaySmoothing)
  LL <- length(Lag)
  #array(numeric(),c(SDL*DSL*LL))
  Matrix=vector(mode="numeric", length=SDL*DSL*LL)
  for (j in 1:DSL){
    for (k in 1:LL){
      for (i in 1:SDL){
        Matrix[j*SDL*LL+k*SDL+i-SDL*LL-SDL]=PlotingOptions(DF=DF,StartDate[i],
                                        DaySmoothing[j],Lag[k],
                                        Ret=Ret,CasesUsed=CasesUsed,
                                        DateStart=DateStart,Pop=Pop)
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
  ListInputs <- LocInput(Matrix,Loc,StartDate,DaySmoothing,Lag)

  stopifnot(Target==PlotingOptions(DF=DF,ListInputs[1],ListInputs[2],ListInputs[3],
                                    Ret=Ret,CasesUsed=CasesUsed,
                                    DateStart=DateStart,Pop=Pop))
  BestLM <- PlotingOptions(DF=DF,ListInputs[1],ListInputs[2],ListInputs[3],Show=Show2,
                           Ret="LM",CasesUsed=CasesUsed,
                           DateStart=DateStart,Pop=Pop)
  
  SlopeL <- signif(BestLM[[1]][1],3)
  DayOfWeekData <- weekdays(seq(as.Date("11/10/2020"), by=1, len=8))
  

  Ret <- paste("Best relationship at",DayOfWeekData[ListInputs[1]+1],ListInputs[2],ListInputs[3],"with", Ret ,"of",
                max(Matrix),"with F factor of",SlopeL)

  if(Mat){
    return(list(Matrix,Ret))
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


PlotingOptions <- function(DF,StartDate,DaySmoothing,Lag,
                           Show=FALSE,Ret="LM",CasesUsed="Cases4",
                           DateStart=mdy("11/1/2020"),Pop=FALSE){
  MadData <- DF%>%
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
  if(Pop){
    MadData$CasesMain <- MadData$CasesMain/mean(MadData$Pop)
  }
  
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
    MaxX <- .9*max(MadData$CasesMain,na.rm=TRUE)
    CompPlot <- MadData%>%
      ggplot()+
      aes(x=CasesMain,y=NM)+
      geom_point()+
      geom_abline(aes(color="Line of best first",slope=1/Slope,intercept=0))+
      labs(x="7 Day binning of SLD Cases",
           y="7 day binning of N1")+
      annotate("text", x=MaxX, y=4e4, label= paste("R^2:", R2))+
      annotate("text", x=MaxX, y=1e5, label= paste("Cor:", COR))
    
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


HeatMapCor <- function(DF,StartDate=0:7,DaySmoothing=c(7),Lag=-2:2,ShowPlots=FALSE,CasesUsed="Cases4",
                       DateStart=mdy("10/1/2020"),Pop=FALSE){
  Site=unique(DF$Site)
  R2CF <- CheckFunction(DF=DF,DaySmoothing=DaySmoothing,StartDate=StartDate,
                        Lag=Lag,Show2=ShowPlots,Mat=TRUE,Ret="R2",Pop=Pop,
                        CasesUsed=CasesUsed,DateStart=DateStart)
  PValCF <- CheckFunction(DF=DF,DaySmoothing=DaySmoothing,StartDate=StartDate,
                          Lag=Lag,Show2=ShowPlots,Mat=TRUE,Ret="PVal",Pop=Pop,
                          CasesUsed=CasesUsed, DateStart=DateStart)
  CorCF <- CheckFunction(DF=DF,DaySmoothing=DaySmoothing,StartDate=StartDate,
                         Lag=Lag,Show2=ShowPlots,Mat=TRUE,Ret="COR",Pop=Pop,
                         CasesUsed=CasesUsed,DateStart=DateStart)
  R2Mat <- matrix(R2CF[[1]],
                  nrow=length(StartDate))
  
  PValMat <- matrix(PValCF[[1]],
                    nrow=length(StartDate))
  
  CorMat <- matrix(CorCF[[1]],
                   nrow=length(StartDate))
  print(paste("R2:",R2CF[[2]]))
  print(paste("PVal:",PValCF[[2]]))
  print(paste("Cor:",CorCF[[2]]))
  
  DayOfWeekData <- weekdays(seq(as.Date("11/10/2020"), by=1, len=8))
  
  xAxisPlot <- expand.grid(DaySmoothing, Lag)
  xAxisPlot <- xAxisPlot[order(xAxisPlot$Var1),]
  
  AxisPattern<- apply(xAxisPlot, 1, paste, collapse=" ")
  HeatMapMaker(R2Mat,AxisPattern,DayOfWeekData,Site=Site,
               "R2 relationship",ColorName="YlOrRd")
  HeatMapMaker(PValMat,AxisPattern,DayOfWeekData, Site=Site,
               "PVal relationship",ColorName="YlOrRd")
  HeatMapMaker(CorMat,AxisPattern,DayOfWeekData, Site=Site,
               "Cor relationship",ColorName="YlOrRd")
}

HeatMapMaker <- function(Mat,ColNames,RowNames,Main,ColorName,Site){
  ColorLegend <- brewer.pal(n = 3, name = ColorName)
  Color <- brewer.pal(n = 8, name = ColorName)
  rownames(Mat) <- RowNames
  colnames(Mat) <- ColNames
  heatmap(Mat,Rowv=NA,Colv=NA,col = Color ,main=Main)
  legend(x="right", legend=c(signif(min(Mat),2),
                             signif(median(Mat),2),
                             signif(max(Mat),2)),
         fill=ColorLegend)
  title(xlab="time laged",ylab="Binning Start",sub=Site)
}



BestCorDFGen <- function(Site,DateFilt=mdy("9/15/2020"),
                         keep=c("N1","N1Error","N2","N2Error","Pop")){
  SiteLimsDF <- DataPrep(LIMSFullDF,
                         keep=keep,
                         Site)
  
  SCPDF <- DFSmoothingFNC(FullCase,SiteS=Site)%>%
    mutate(Site=Site)
  
  SCPDF2 <- DFSmoothingFNC(FullCase,PreRoll=TRUE,SiteS=Site)%>%
    mutate(Site=Site)
  
  SCPDF3 <- inner_join(SCPDF,SCPDF2,by=c("Date","Site","Cases"),suffix=c("",".PreRolled"))
  
  MergedDF <- full_join(SCPDF3,SiteLimsDF, by=c("Date","Site"))
  
  MergedDF$LoessN1 <- exp(DFLoessFNC(MergedDF,Var="N1",SiteS=Site,span=.2))
  MergedDF$LoessN2 <- exp(DFLoessFNC(MergedDF,Var="N2",SiteS=Site,span=.2))
  
  MergedDF$N1Filtered <- ReplacementFilter(6,MergedDF$N1,MergedDF$LoessN1)
  MergedDF <- MergedDF%>%
    filter(Date>DateFilt)%>%
    select(Date,Site,Cases,Cases2,Cases2.PreRolled,
           LoessN1,LoessN2,N1Filtered,keep)
}
