library(tidyverse)
library(lubridate)
library(readxl)



#Creates  CovidNumberData

CovidData = function(CovidFileName){
  lag=1
  covidData = read.csv(CovidFileName)%>%
    mutate(ServiceID = ifelse(ServiceID=="MMSD","Madison",paste("MMSD P",ServiceID,sep="")),Date = as.Date(Date))%>%
    rename(Site=ServiceID)%>%
    group_by(Site)%>%
    mutate(Cases=Cases - lag(Cases, n=lag,default = NA),Tests=Tests- lag(Tests, n=lag,default = NA))%>%
    mutate(roll=Cases/Tests)
  return(covidData)
}


CovidDataDorms= function(File1,File2){
  CaseData1=read.csv(File1,sep = "",header = T)
  CaseData2=read.csv(File2,sep = "",header = T)
  returnedData=rbind(CaseData1,CaseData2)%>%
    mutate(Tests=Negative	+Positive)%>%
    mutate( Site = ifelse(Site == "UW_D", "UW-LakeShore", "UW-Sellery"))%>%
    mutate(Date=mdy(Date))%>%
    rename(Cases=Positive)
  return(returnedData)
}


HFGCasesPARSER = function(data){
  DataF=read.csv(data)%>%
    rename(Plant=wwtp_name,Date=SpecCollectedDate)%>%
    mutate(Date=mdy(Date))
  return(DataF)
}