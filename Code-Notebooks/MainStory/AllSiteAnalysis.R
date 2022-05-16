  ## Setup ---------------
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
  
  defaultArgs <- list (
    BaseDir = "../../",
    WasteVar = "N1FlowPop",
    OutFile = "AllPlotOutputN1Log.PDF",
    verbose = FALSE
  )
  
  args <- R.utils::commandArgs(trailingOnly = TRUE,
                               asValues = TRUE ,
                               defaults = defaultArgs)
  
  
  ##Data importing -------------

  #Importing the case data
  LatCaseDF <- MainCaseDataPrep(args$BaseDir,"")

  #Importing the waste water data
  LIMSFullDF <- MainWastePrep(args$BaseDir,"")
  
  #joining the two data frames together
  FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))%>%
    filter(!is.na(Cases))
  
  #Break code into sites for date based transformations
  SiteDFList <- split(FullDF, FullDF$Site)
  #Add Loess WasteSmoothing smoothing and SLD case smoothing
  SiteDFList.ad <- lapply(SiteDFList, DataProcess, 21,  args$WasteVar, "guess",args$verbose)
  #add quintile ranking param the code into 
  SiteDFList.ad <- lapply(SiteDFList.ad,NormThird,  args$WasteVar,"SevenDayMACases","loVar",  args$WasteVar)
  
  DataMod <- bind_rows(lapply(SiteDFList.ad,NormQuint,"loVar","SevenDayMACases","loVar"))
  
  
  

#Order Sites by number of points  
  DataMod <- FactorVecByNumPoints(DataMod, "Site",   args$WasteVar)
  
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
  
  ggsave(args$OutFile, plot=Gplt,path="RmdOutput",
         width = 32,height=100,units="cm")

  