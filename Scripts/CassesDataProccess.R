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


CovidDataPARSER= function(File1,File2,MMSDFN){
  CaseData1=read.csv(File1,sep = "",header = T)
  CaseData2=read.csv(File2,sep = "",header = T)
  returnedData=rbind(CaseData1,CaseData2)
  returnedData=returnedData%>%
    mutate(Tests=Negative	+Positive,Per_pos=100*Positive/Tests)%>%
    mutate( Site = ifelse(Site == "UW_D", "UW-LakeShore", "UW-Sellery"))%>%
    mutate(Date=mdy(Date))%>%
    rename(Cases=Positive)%>%
    select(Date,Site,Cases,Tests,Per_pos)
  
  lag=1
  MMSDdata = read.csv(MMSDFN)%>%
    mutate(ServiceID = ifelse(ServiceID=="MMSD","Madison",paste("MMSD-P",ServiceID,sep="")),Date = as.Date(Date))%>%
    rename(Site=ServiceID)%>%
    group_by(Site)%>%
    mutate(Cases=Cases - lag(Cases, n=lag,default = NA),Tests=Tests- lag(Tests, n=lag,default = NA))%>%
    mutate(Per_pos=100*Cases/Tests)%>%
    select(Date,Site,Cases,Tests,Per_pos)
  LatCaseDF=rbind(returnedData,MMSDdata)%>%
    mutate(Site = ifelse(Site == "MMSD P18", "MMSD-P18", Site),
           Site = ifelse(Site == "MMSD P11", "MMSD-P11", Site),
           Site = ifelse(Site == "MMSD P2", "MMSD-P2", Site),
           Site = ifelse(Site == "MMSD P7", "MMSD-P7", Site),
           Site = ifelse(Site == "MMSD P8", "MMSD-P8", Site))
    
  return(LatCaseDF)
}


HFGCasesPARSER = function(data){
  DataF=read.csv(data)%>%
    rename(Site=wwtp_name)%>%
    filter(!is.na(Site))%>%
    mutate(Date=ymd(Date))%>%
    mutate(Site=ifelse(Site=="SunPrairie","Sun Prairie",Site))%>%
    mutate(Site=ifelse(Site=="RiverFalls","River Falls",Site))
  return(DataF)
}