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


CovidDataPARSER= function(File1=NA,File2=NA,MMSDFN=NA){
  if(is.na(File1)&&is.na(File2)&&is.na(MMSDFN)){
    return(NA)
  }
  DormDF <- NA
  if(!is.na(File1)){
    DormDF <- DormCaseDataPARSER(File1)
  }
  if(!is.na(File2)){
    DormDF2 <- DormCaseDataPARSER(File2)
  }
  if(!is.na(File2)&&!is.na(File1)){
    DormDF=rbind(DormDF,DormDF2)
  }else if(!is.na(File2)){
    DormDF=DormDF2
  }
  if(!is.na(MMSDFN)){
    lag=1
    MMSDdata = read.csv(MMSDFN)%>%
      rename(Site=ServiceID)%>%
      mutate(Site = ifelse(Site=="MMSD","Madison",paste("MMSD-P",Site,sep="")),
             Date = as.Date(Date))%>%
      group_by(Site)%>%
      mutate(Cases=Cases - lag(Cases, n=lag,default = NA),Tests=Tests- lag(Tests, n=lag,default = NA))%>%
      mutate(Per_pos=100*Cases/Tests)
  } else {
    FullData <- DormDF
    MMSDdata <- NA
  }
  if(!is.na(DormDF)&&!is.na(MMSDdata)){
      FullData <- rbind(DormDF,MMSDdata)
  } else if(is.na(DormDF)){
    FullData <- MMSDdata 
  }
      
  FullData <- FullData%>%
    mutate(Site = ifelse(Site == "MMSD P18", "MMSD-P18", Site),
           Site = ifelse(Site == "MMSD P11", "MMSD-P11", Site),
           Site = ifelse(Site == "MMSD P2", "MMSD-P2", Site),
           Site = ifelse(Site == "MMSD P7", "MMSD-P7", Site),
           Site = ifelse(Site == "MMSD P8", "MMSD-P8", Site))
    
  return(FullData)
}
#%>%select(Date,Site,Cases,Tests,Per_pos)



DormCaseDataPARSER <- function(FileName){
  CSVDF <- read.csv(FileName,sep = "",header = T)%>%
    mutate(Tests=Negative	+Positive,Per_pos=100*Positive/Tests)%>%
    mutate(Site = ifelse(Site == "UW_D", "UW-LakeShore", Site))%>%
    mutate(Site = ifelse(Site == "UW_S", "UW-Sellery", Site))%>%
    mutate(Date=mdy(Date))%>%
    rename(Cases=Positive)%>%
    select(-Negative)%>%
    mutate(Population=NA)
  return(CSVDF)
}




HFGCasesPARSER = function(FN){
  HFGCaseDFNoReported <- read.csv(FN)%>%
    rename(Site=wwtp_name)%>%
    filter(!is.na(Site))%>%
    mutate(Date=ymd(Date))%>%
    mutate(Site=ifelse(Site=="SunPrairie","Sun Prairie",Site))%>%
    mutate(Site=ifelse(Site=="RiverFalls","River Falls",Site))
  
  HFGCaseDFReportedOnly <- HFGCaseDFNoReported%>%
    mutate(Date=Date+1,ReportedCases=ConfirmedCases)%>%
    select(Date,Site,ReportedCases)
  
  HFGCaseDF  <-  full_join(HFGCaseDFReportedOnly,HFGCaseDFNoReported,by=c("Date","Site"))
  return(HFGCaseDF)
}