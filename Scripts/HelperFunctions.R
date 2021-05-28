RollAvg = function(vectorPerPos,n=7){
  CurrNum = rep(NA,n)
  SlideMeanVec=vector(mode="double", length=length(vectorPerPos))
  for (i in 1:length(vectorPerPos)){
    
    CurrNum[(i-1)%%n+1]=vectorPerPos[i]
    SlideMeanVec[i]=mean(CurrNum,na.rm=T)
  }
  return(SlideMeanVec)
}
RollPerPos = function(RollingDF,CaseName,TestName,Fucet=NA,n=7){
  FulldayRange=data.frame(Date=seq.Date(min(RollingDF$Date),max(RollingDF$Date),1))
  FullDataFM=full_join(RollingDF,FulldayRange,by=c("Date"))%>%
    arrange(!!sym(Fucet),Date)%>%
    group_by(!!sym(Fucet))%>%
    mutate(rollingPer_pos=RollPerPosHelperFunc(!!sym(CaseName),!!sym(TestName)),SevCases=RollAvg(!!sym(CaseName)))%>%
    ungroup()
  return(FullDataFM)
}


RollPerPosHelperFunc = function(vectorCases,vectorTests,n=7){
  CurrNumCase = rep(NA,n)
  CurrNumTests = rep(NA,n)
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    CurrNumCase[(i-1)%%n+1]=vectorCases[i]
    CurrNumTests[(i-1)%%n+1]=vectorTests[i]
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