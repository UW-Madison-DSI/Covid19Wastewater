RollAvg = function(vectorPerPos,n=7){
  CurrNum = rep(NA,n)
  SlideMeanVec=vector(mode="double", length=length(vectorPerPos))
  for (i in 1:length(vectorPerPos)){
    
    CurrNum[(i-1)%%n+1]=vectorPerPos[i]
    SlideMeanVec[i]=mean(CurrNum,na.rm=T)
  }
  return(SlideMeanVec)
}
RollPerPos = function(vectorCases,vectorTests,n=7){
  CurrNumCase = rep(NA,n)
  CurrNumTests = rep(NA,n)
  SlideMeanVec=vector(mode="double", length=length(vectorCases))
  for (i in 1:length(vectorCases)){
    CurrNumCase[(i-1)%%n+1]=vectorCases[i]
    CurrNumTests[(i-1)%%n+1]=vectorTests[i]
    SlideMeanVec[i]=100*sum(CurrNumCase,na.rm=T)/sum(CurrNumTests,na.rm=T)
  }
  return(SlideMeanVec)
}