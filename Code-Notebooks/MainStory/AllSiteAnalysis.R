defaultArgs <- list (
  BaseDir = "../../",
  WasteVar = "sars_cov2_adj_load",
  OutFile = "AllPlotLoessSmoothMod.PDF",
  log = FALSE,
  verbose = FALSE
)

args <- R.utils::commandArgs(trailingOnly = TRUE,
                             asValues = TRUE ,
                             defaults = defaultArgs)


  ## Setup ---------------
  library(dplyr, warn.conflicts=FALSE)
  library(ggplot2, warn.conflicts=FALSE)
  library(lmtest, warn.conflicts=FALSE)
  library(lubridate, warn.conflicts=FALSE)
  library(limma, warn.conflicts=FALSE)
  library(tidyr, warn.conflicts=FALSE)
  library(plotly, warn.conflicts=FALSE)
  library(gridExtra, warn.conflicts=FALSE)
  library(data.table, warn.conflicts=FALSE)
  
  
  #Data Files and prep work
  source("../../lib/DataPathName.R")
  source("../../lib/TSTrendGen.R")
  source("../../lib/OutlierDetectionFuncs.R")
  source("MainStory.R")
  
  
  ##Data importing ------------
  
  #Importing the waste water data
  LIMSFullDF <- read.csv(LIMSWastePath(args$BaseDir))%>%
    #rename(Date = date, Site = WWTP)%>%
    mutate(Date = as.Date(Date),
           sars_cov2_adj_load = sqrt(N1*N2)*FlowRate/Pop)%>%
    filter(!is.na(Date), !is.na(!!sym(args$WasteVar)))%>%
  group_by(Site)%>%
  filter(n()>120)%>%
  group_split()%>%
    lapply(TrendSDOutlierFilter, args$WasteVar, 2.5, 36, n = 5, 
           TrendFunc = LoessSmoothMod, verbose=args$verbose)%>%
    lapply(LoessSmoothMod, args$WasteVar, "loVar", Filter = "FlaggedOutlier")%>%
    bind_rows()
#ExpSmoothMod
#sgolaySmoothMod
#LoessSmoothMod

#Importing the case data
LatCaseDF <- MainCaseDataPrep(args$BaseDir,"")%>%
  filter(Site %in% unique(LIMSFullDF$Site))%>%
  group_by(Site)%>%
  group_split()%>%
  lapply(SLDSmoothMod,21)%>%
  bind_rows()

LIMSFullDF <- LIMSFullDF%>%
  filter(Site %in% unique(LatCaseDF$Site))
  
  #joining the two data frames toether
  FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))%>%
    group_by(Site)%>%
    group_split()%>%
    lapply(NormThird,  args$WasteVar,"SevenDayMACases", "loVar",  args$WasteVar)%>%
    lapply(NormQuint, "loVar", "SevenDayMACases", "loVar")%>%
    bind_rows()%>%
    filter(!is.na(Site))

  
  

#Order Sites by number of points  
FullDF <- FactorVecByNumPoints(FullDF, "Site",   args$WasteVar)
  
  Gplt <- FullDF%>%
    ggplot(aes(x=Date))+
    geom_line(aes(y=SevenDayMACases,
                  color="Seven Day MA Cases"),data=filter(FullDF,!is.na(SevenDayMACases)))+
    geom_line(aes(y=loVar, 
                  color="LoessSmooth"),data=filter(FullDF,!is.na(loVar)))+
    #geom_point(aes(y=get(args$WasteVar),color="BLOD"),size=.5,data=filter(FullDF,N1LOD))+
    geom_point(aes(y=get(args$WasteVar),color="Flagged Outliers"),size=.5,data=filter(FullDF,FlaggedOutlier))+
    scale_x_date(date_labels = "%b %y") +
    facet_wrap(~Site,scales="free",ncol=4)#should be more systematic
    
  if(as.logical(args$log)){
    Gplt <- Gplt + 
      scale_y_log10()
  }
  
  ggsave(args$OutFile, plot=Gplt, path="RmdOutput",
         width = 32, height=20, units="cm")

  