MainCaseDataPrep <- function(Dir,SiteC){
  DF <- ParseData(LIMSCasePath(Dir))%>% 
    rename(Cases = FirstConfirmed)%>%
    mutate(Cases = ifelse(is.na(Cases),0,Cases))
  if(SiteC!=""){
    DF2 <- DF%>%
      filter(Site %in% SiteC)
  }else{
    DF2 <- DF
  }
  DF2 <- DF2%>%
    group_by(Site)%>%
    arrange(Date)%>%
    mutate(SevenDayMACases = rollapply(data = Cases, width = 14, FUN = mean, 
                                       partial = TRUE))%>%
    select(Date, Site, Cases,SevenDayMACases)
  return(DF2)
}

MainWastePrep <- function(Dir,SiteC){
  DF <- ParseData(LIMSWastePath(Dir))
  if(SiteC!=""){
    DF2 <- DF%>%
      filter(Site %in% SiteC)
  }else{
    DF2 <- DF
  }
  DF2 <- DF2%>%
    mutate(N1Pure = ifelse(is.na(N1),N2,N1),
           N2Pure = ifelse(is.na(N2),N1,N2),
           GeoMeanN12 = exp((log(N1Pure)+log(N2Pure))/2),
           N1FlowPop = Pop*N1/FlowRate)%>%
    select(Date, Site, N1 , N2 , GeoMeanN12, N1FlowPop,Pop,N1LOD)
  return(DF2)
}#mutate(N1FlowPop = ifelse(is.na(N1FlowPop),N1*mean(FlowRate,na.rm=TRUE)/Pop,N1FlowPop))


SLDSmoothMod <- function(DF, Width){
  Mean <- 11.73
  StandardDeviation <- 7.68
  Scale = StandardDeviation^2/Mean
  Shape = Mean/Scale

  weights <- dgamma(1:Width, scale = Scale, shape = Shape)
  ModDF <- DF%>%
    arrange(Date)%>%
    mutate(
      SLDCases = c(rep(NA,Width-1), #elimination of starting values not relevant
                   #as we have a 50+ day buffer of case data
                   rollapply(Cases,width=Width,FUN=weighted.mean,
                             w=weights,
                             na.rm = TRUE)
                   #,rep(NA,10)
      ))#no missing data to remove
  return(ModDF)
}

spanGuess <- function(DF,InVar){
  temp <- DF%>%
    filter(!is.na(!!sym(InVar)))%>%
    summarise(n=n())
  span <- min(c(.1*178/temp$n,.6))#More can be done here
  return(span)
}

LoessSmoothMod <- function(DF,InVar, OutVar, span){
  if(span=="guess"){
    span <- spanGuess(DF,InVar)
  }
  DF2 <- DF%>%
    arrange(Date)
  DF2[[OutVar]] <- loessFit(y=(DF2[[InVar]]), 
                                        x=DF2$Date, #create loess fit of the data
                                        span=span, 
                                        iterations=2)$fitted#2 iterations remove some bad patterns
  return(DF2)
}

DataProcess <- function(DF, Width,InVar, span){
  if(span=="guess"){
    span <- spanGuess(DF,InVar)
  }
  #DF,VecName,SDDeg,span,DaySmoothed,n = 5
  DetectedOutliers <- LoessOutlierDetec(DF,InVar,2.5,2*span,36,n=5)
  print(sum(DetectedOutliers)/length(DF[[InVar]])*100)
  ErrorRemovedDF <- DF[!(DetectedOutliers),]%>%
    mutate(FlaggedOutlier=FALSE)
  OutlierDF <- DF[(DetectedOutliers),]%>%
    mutate(loVar=NA,SLDCases=NA,FlaggedOutlier=TRUE)
  if(length(ErrorRemovedDF$N1)<Width+1){
    RetDF <- ErrorRemovedDF%>%
      mutate(loVar=NA,SLDCases=NA)%>%
      filter(!is.na(loVar))
    return(RetDF)
  }
  SLDDF <- SLDSmoothMod(ErrorRemovedDF,Width)
  LoessDF <- LoessSmoothMod(SLDDF,InVar, "loVar", span)
  RetDF <- OutlierDF%>%
    bind_rows(LoessDF)
  return(RetDF)
}


MinMaxSiteFixing <- function(Vec, Bar){
  
  NormVec = MinMaxNormalization(Vec,MinMaxCollector(SubVec))
  BarRange = MinMaxCollector(Bar)
  RefitVec = NormVec*(BarRange[2]-BarRange[1])+BarRange[1]
  return(RefitVec)
}



NormWithVec <- function(Vec1, Levels){
  
  return(((Vec1-Levels[1])/
                     (Levels[2]-Levels[1]))*
                      (Levels[4]-Levels[3])+
                      Levels[3])
}


MinMaxCalc <- function(Vec){
  return(c(min(Vec,na.rm=TRUE),max(Vec,na.rm=TRUE)))
}

QuintCalc <- function(Vec,low,high){
  return(c(quantile(Vec,low,na.rm=TRUE),quantile(Vec,high,na.rm=TRUE)))
}



NormOP <- function(DF,Vec1Name,RetName,Levels){
  Vec1 <- pull(DF,!!Vec1Name)
  RetDF <- DF
  RetDF[[RetName]] <- NormWithVec(Vec1,Levels)
  return(RetDF)
}

NormMinMax <- function(DF,Vec1Name,Vec2Name,RetName){
  Vec1 <- pull(DF,!!Vec1Name)
  Vec2 <- pull(DF,!!Vec2Name)
  Levels <- c(MinMaxCalc(Vec1),MinMaxCalc(Vec2))
  return(NormOP(DF,Vec1Name,RetName,Levels))
}

NormQuint <- function(DF,Vec1Name,Vec2Name,RetName){
  Vec1 <- pull(DF,!!Vec1Name)
  Vec2 <- pull(DF,!!Vec2Name)
  Levels <- c(QuintCalc(Vec1,.1,.9),QuintCalc(Vec2,.1,.9))
  return(NormOP(DF,Vec1Name,RetName,Levels))
}

NormThird <- function(DF,Vec1Name,Vec2Name, Vec3Name,RetName){
  Vec3 <- pull(DF,!!Vec3Name)
  Vec2 <- pull(DF,!!Vec2Name)
  Levels <- c(QuintCalc(Vec3,.1,.9),QuintCalc(Vec2,.1,.9))
  return(NormOP(DF,Vec1Name,RetName,Levels))
}



LoessOutlierDetec <- function(DF,VecName,SDDeg,span,DaySmoothed,n = 5){

  FullDateRange <- data.frame(Date=seq(min(DF$Date),max(DF$Date),by ="day"))
  
  BestVectorDF <- DF%>%
    mutate(VecName = log(!!sym(VecName)))
  
  for(i in 1:n){#robustly remove outliers and recalc smooth line
    #DF,InVar, OutVar, span
    BestVectorDF <- LoessSmoothMod(BestVectorDF,"VecName","Temp",span)%>%
      full_join(FullDateRange,by="Date")%>%
      mutate(SD = rollapply(VecName-Temp,DaySmoothed,sd,na.rm=TRUE,partial=TRUE),
        VecName = ifelse(VecName>Temp+SDDeg*SD,Temp,VecName),
        VecName = ifelse(VecName<Temp-2*SDDeg*SD,Temp,VecName))%>%
      filter(Date %in% DF$Date)
  }
  
  
  booleanReturn <- abs(exp(BestVectorDF$VecName)-DF[[VecName]])>10
  booleanReturn[is.na(booleanReturn)] <- FALSE
  return(booleanReturn)
}


FactorVecByNumPoints <- function(DF,FacVar, FiltVar){
  FactorOrder <- (DF%>%
                    filter(!is.na(!!sym(FiltVar)))%>%
                    group_by(!!sym(FacVar))%>%
                    summarise(n=n())%>%
                    arrange(desc(n)))[[FacVar]]
  
  
  FacedDF <- DF%>%
    mutate(!!FacVar := factor(!!sym(FacVar),FactorOrder))
  
  return(FacedDF)
}
