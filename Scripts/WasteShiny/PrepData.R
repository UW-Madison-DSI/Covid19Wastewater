#Longatudal Data

LatCaseDF=CovidDataPARSER(LatSpringCaseFN,LatFallCaseFN,LatMMSDFN)%>%
  filter(!is.na(Site))%>%
  select(Date,Site,Cases,Tests,Per_pos)

#return(LatCaseDF)
LatCaseDFRoll=RollPerPos(LatCaseDF,"Cases","Tests",Facet="Site")%>%
  select(Date,Site,Cases,Per_pos)

LatWasteDF=WasteWater(LatWasteFN)%>%
  mutate(Date=as.Date(Date))%>%
  filter(!is.na(Date),!is.na(N1),!is.na(Site))%>%
  filter(Date<mdy("6/5/2021"))%>%
  select(Date,Site,N1,N2,PMMoV,Pct_BCoV,AVG)



#Reads Transformed HFG Data

HFGFrame=HFGInfo(HFGWasteFN)%>%
  select(Date,Site=Plant,Filter,Well,N1=N1GC,N2=N2GC,PMMoV=PMMOVGC,Pct_BCoV=BCoV)%>%
  mutate(Filter=as.character(Filter),Well=as.character(Well))%>%
  rename(`Filter replicates`=Filter)


HFGCaseDF=HFGCasesPARSER(HFGCaseFN)



HFGCaseDFRoll=HFGCaseDF%>%
  mutate(EpisodeCases=ifelse(EpisodeCases==-999,2.5,EpisodeCases))%>%
  mutate(CollectedCases=ifelse(CollectedCases==-999,2.5,CollectedCases))%>%
  mutate(ConfirmedCases=ifelse(ConfirmedCases==-999,2.5,ConfirmedCases))%>%
  RollAvg(Facet="Site")%>%
  select(Date,Site,ends_with("Cases"))

HFGPop=read.csv("../../UntrackedData/HFGPop.csv.txt")%>%
  select(Site=wwtp_name,Population=pop_served)

HFGCaseDF=full_join(HFGPop,HFGCaseDF,by="Site")

HFGCaseDFRoll=full_join(HFGPop,HFGCaseDFRoll,by="Site")

#Generating the weekends starts and end dates
#TO DO:Capture the weekend if the data intersects with it
HFGDateRangeDF=WeekendGen(HFGCaseDF$Date)


