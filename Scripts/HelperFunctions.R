
RollPerPos = function(RollingDF,CaseName,TestName,Facet=NA,n=7){
  TDF=RollingDF%>%
    mutate(Facet=!!sym(Facet),CaseName=!!sym(CaseName),TestName=!!sym(TestName))
  FaucetOptions=unique(TDF$Facet)
  FulldayRange=expand.grid(seq.Date(min(TDF$Date),max(TDF$Date),1),FaucetOptions)%>%
    rename(Date=Var1,Facet=Var2)
  
  FullDataFM=full_join(TDF,FulldayRange,by=c("Date","Facet"))%>%
    arrange(Facet,Date)%>%
    group_by(Facet)%>%
    mutate(Per_pos=RollPerPosHelperFunc(CaseName,TestName,n=n),CaseName=RollAvgHelperFunc(CaseName,n=n))%>%
    ungroup()
  FullDataFM[[CaseName]]=FullDataFM$CaseName
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

RollAvg = function(RollingDF,Facet="Site",n=7,var=c("EpisodeCases","CollectedCases","ConfirmedCases")){
  TDF=RollingDF%>%
    mutate(Facet=!!sym(Facet))
  FaucetOptions=unique(TDF$Facet)
  FulldayRange=expand.grid(seq.Date(min(TDF$Date),max(TDF$Date),1),FaucetOptions)%>%
    rename(Date=Var1,Facet=Var2)
  
  FullDataFM=full_join(TDF,FulldayRange,by=c("Date","Facet"))%>%
    arrange(Facet,Date)%>%
    group_by(Facet)%>%
    mutate(across(any_of(var),RollAvgHelperFunc,n=n))%>%
    ungroup()
  FullDataFM[[Facet]]=FullDataFM$Facet
  FullDataFM=FullDataFM%>%
    select(-Facet)
  return(FullDataFM)
}

RollAvgHelperFunc = function(vectorCases,n=7){
  CurrNumCase = rep(NA,n)
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    CurrNumCase[(i-1)%%n+1]=vectorCases[i]
    SlideMeanVec[i]=mean(CurrNumCase,na.rm=T)
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