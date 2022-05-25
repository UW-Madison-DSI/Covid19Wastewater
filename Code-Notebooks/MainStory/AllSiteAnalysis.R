defaultArgs <- list (
  BaseDir = "../../",
  WasteVar = "sars_cov2_adj_load",
  OutFile = "AllPlotLoessSmoothMod.PDF",
  log = FALSE,
  verbose = TRUE
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
  #filter(lab_submitter=="SLH")%>%
    #rename(Date = date, Site = WWTP)%>%
    mutate(Date = as.Date(Date),
           sars_cov2_adj_load = sqrt(N1*N2)*FlowRate/Pop)%>%
    filter(!is.na(Date), !is.na(!!sym(args$WasteVar)))%>%
  group_by(Site)%>%
  #filter(n()>120)%>%
  group_split()%>%
    lapply(TrendSDOutlierFilter, args$WasteVar, 1.5, 13, n = 5, 
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
  
  #joining the two data frames together
  FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))%>%
    group_by(Site)%>%
    group_split()%>%
    lapply(NormThird,  args$WasteVar,"SevenDayMACases", "loVar",  args$WasteVar)%>%
    lapply(NormQuint, "loVar", "SevenDayMACases", "loVar")%>%
    bind_rows()%>%
    #filter(Date>mdy("7/15/2021"))%>%
    filter(!is.na(Site))%>%
    FactorVecByNumPoints("Site",  args$WasteVar)
  
  
  CCFDF <- FullDF%>%
    group_by(Site)%>%
    group_split()%>%
    lapply(MaxCFDFGen, "SevenDayMACases", "loVar")%>%
    bind_rows()%>%
    FactorVecByNumPoints("Site",   "Site")
  
  

CatagoryColors <- c("#880808","#EE4B2B", "#D2042D","#6495ED")


  Gplt <- FullDF%>%
    
    ggplot(aes(x=Date))+
    
    geom_point(aes(y=get(args$WasteVar), color=args$WasteVar),
               size=.5, alpha = .2)+
    
    geom_line(aes(y=SevenDayMACases,
                  color="Seven Day MA Cases"), data=filter(FullDF,!is.na(SevenDayMACases)))+
    
    geom_line(aes(y=loVar, 
                  color="LoessSmooth"),data=filter(FullDF,!is.na(loVar)))+
    
    #geom_point(aes(y=get(args$WasteVar),color="BLOD"),size=.5,data=filter(FullDF,N1LOD))+
    
    geom_point(aes(y=get(args$WasteVar),color="Flagged Outliers"),size=.5, data=filter(FullDF,FlaggedOutlier))+
    
    geom_label(aes( label = paste("Cor:",round(cor,2)), y = yPos/1.5), x = mdy("7/15/2021"), size=3,data = CCFDF)+
    
    geom_label(aes( label = paste("Shift:", lag), y = yPos*1.25/1.5), x = mdy("7/15/2021"), size=3,data = CCFDF)+
    
    
    scale_x_date(date_labels = "%b %y") +
    
    scale_color_manual(values = CatagoryColors)+
    
    facet_wrap(~Site,scales="free",ncol=4)
    
  

  if(as.logical(args$log)){
    Gplt <- Gplt + 
      scale_y_log10()
  }
  
  ggsave(args$OutFile, plot=Gplt, path="RmdOutput",
         width = 32, height=80, units="cm")

  