library(dplyr)
library(ggplot2)
library(lmtest)
library(lubridate)
library(limma)
library(tidyr)
library(plotly)
library(gridExtra)
library(data.table)
library(formattable)

#Data Files and prep work
source("../../lib/DataProccess.R")
source("../../lib/NormFuncs.R")
source("../../lib/OutlierDetectionFuncs.R")
source("../../lib/DataPathName.R")
source("MainStory.R")

  
#Importing the Madison case data
LatCaseDF <- MainCaseDataPrep(BaseDir,"")

#Importing the Madison waste water data
LIMSFullDF <- MainCaseWastePrep(BaseDir,"")

#joining the two data frames together
FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))%>%
  filter(Pop > 50000,
         !is.na(Cases))

FactorOrder <- (FullDF%>%
  group_by(Site)%>%
  summarise(Pop=mean(Pop,na.rm=TRUE))%>%
  arrange(desc(Pop)))$Site

SiteDFList <- split(FullDF, FullDF$Site)#N1FlowPop
DataMod <- bind_rows(lapply(SiteDFList, DataProcess, 21,"N1FlowPop", "guess"))

FactorOrder <- (DataMod%>%
                  group_by(Site)%>%
                  summarise(Pop=mean(Pop,na.rm=TRUE))%>%
                  arrange(desc(Pop)))$Site

DataMod <- DataMod%>%
  mutate(Site = factor(Site,FactorOrder))


Gplt <- DataMod%>%
  ggplot(aes(x=Date))+
  geom_line(aes(y=Cases, color="Cases" ),alpha=.1)+
  geom_line(aes(y=MinMaxFixing(N1FlowPop,SevenDayMACases),
                color="N1FlowPop"),
            alpha=.2)+
  geom_line(aes(y=SevenDayMACases,
                color="Seven Day MA Cases"))+
  geom_line(aes(y=MinMaxFixing(loVar,SevenDayMACases,N1FlowPop), 
                color="loVar"))+
  facet_wrap(~Site,scales="free")

ggplotly(Gplt)
