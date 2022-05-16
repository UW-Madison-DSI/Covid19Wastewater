  
  #?add parm script functionality
  BaseDir <- "./../../"
  #Var1 <- ""
  
  library(dplyr)
  library(ggplot2)
  library(lmtest)
  library(lubridate)
  library(limma)
  library(tidyr)
  library(plotly)
  library(gridExtra)
  library(data.table)
  
  #Data Files and prep work
  source("../../lib/DataPathName.R")#merged?
  source("../../lib/DataProccess.R")
  source("MainStory.R")
  
  BaseDir <- "./../../"
  #Importing the case data
  LatCaseDF <- MainCaseDataPrep(BaseDir,"")
  
  #Importing the waste water data
  LIMSFullDF <- MainWastePrep(BaseDir,"")
  
  #joining the two data frames together
  FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))%>%
    filter(!is.na(Cases))
  
  SiteDFList <- split(FullDF, FullDF$Site)
  SiteDFList.ad <- lapply(SiteDFList, DataProcess, 21,"N1FlowPop", "guess")
  SiteDFList.ad <- lapply(SiteDFList.ad,NormThird,"N1FlowPop","SevenDayMACases","loVar","N1FlowPop")
  DataMod <- bind_rows(lapply(SiteDFList.ad,NormQuint,"loVar","SevenDayMACases","loVar"))
  
  
  
  #write DF with date if new?
  #read old DF, if rows are same merge, otherwise make new one?
  #append col if new var used?
  
  DataMod <- FactorVecByNumPoints(DataMod, "Site", "N1FlowPop")
  
  Gplt <- DataMod%>%
    ggplot(aes(x=Date))+
    geom_line(aes(y=SevenDayMACases,
                  color="Seven Day MA Cases"))+
    geom_line(aes(y=loVar, 
                  color="LoessSmooth"),data=filter(DataMod,!is.na(loVar)))+
    geom_point(aes(y=N1FlowPop,color="BLOD"),size=.5,data=filter(DataMod,N1LOD))+
    geom_point(aes(y=N1FlowPop,color="Flagged Outliers"),size=.5,data=filter(DataMod,FlaggedOutlier))+
    scale_y_log10()+
    facet_wrap(~Site,scales="free",ncol=4)#should be more systematic
  
  ggsave("AllPlotOutputN1Log.PDF",plot=Gplt,path="RmdOutput",
         width = 32,height=100,units="cm")

  