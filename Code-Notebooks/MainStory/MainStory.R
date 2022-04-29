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

MainCaseWastePrep <- function(Dir,SiteC){
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
    select(Date, Site, N1 , N2 , GeoMeanN12, N1FlowPop,Pop)
  return(DF2)
}

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

LoessSmoothMod <- function(DF,InVar, OutVar, span){
  DF[[OutVar]] <- loessFit(y=(DF[[InVar]]), 
                                        x=DF$Date, #create loess fit of the data
                                        span=span, 
                                        iterations=2)$fitted#2 iterations remove some bad patterns
  return(DF)
}

DataProcess <- function(DF, Width,InVar, span, OutVar="loVar"){
  ErrorMarkedDF2 <- DF[!(IdentifyOutliers(DF[[InVar]], Action = "Flag")),]
  #print(length(ErrorMarkedDF2$N1))
  if(length(ErrorMarkedDF2$N1)<Width+1){
    RetDF <- ErrorMarkedDF2%>%
      mutate(loVar=NA,SLDCases=NA)%>%
      filter(!is.na(loVar))
    return(RetDF)
  }
  SLDDF <- SLDSmoothMod(ErrorMarkedDF2,Width)
  if(span=="guess"){
    span <- .05#More can be done here
  }
  LoessDF <- LoessSmoothMod(SLDDF,InVar, "loVar", span)
  return(LoessDF)
}