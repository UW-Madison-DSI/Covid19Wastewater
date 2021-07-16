#LatWasteFN <- "../../../UntrackedData/WW SARS-COV-2 Data V5.xlsx"

HFGWasteFN  <-  "../../../UntrackedData/HFG data for stats corrected 071421.xlsx"
HFGCaseFN <- "../../../UntrackedData/HighFreq_CaseData_2021-05-07.csv"
LatMMSDFN  <-  "../../CaseData/results/2021-06-24/MMSD_Cases.csv"
LatSpringCaseFN <- "../../../UntrackedData/SpringSemester_CasesByDorm.tsv"
LatFallCaseFN <- "../../../UntrackedData/FallSemester_CasesByDorm.tsv"
LIMSFN <- "../../../UntrackedData/WATERMICRO_WW_COVID-2021-06-07 19 05.xlsx"

LatCaseDF <- CovidDataPARSER(LatSpringCaseFN,LatFallCaseFN,LatMMSDFN)%>%
  filter(!is.na(Site))%>%
  select(Date,Site,Cases,Tests,Per_pos)


LatCaseDFRoll <- RollPerPos(LatCaseDF,"Cases","Tests",Facet="Site")%>%
  select(Date,Site,Cases,Per_pos)

# LatWasteDF=WasteWater(LatWasteFN)%>%
#   mutate(Date=as.Date(Date))%>%
#   filter(!is.na(Date),!is.na(N1),!is.na(Site))
#%>%
 # select(Date,Site,N1,N2,PMMoV,Pct_BCoV,AVG)

ConfigOption <- list(
  myseed=1234567890,
  XAxisLabSiz=10,
  YAxisLabSiz=15,
  GenFontSiz=20,
  alphaPoint=.7,
  PointSize=2,
  alphaWeek=.2)



missing_codes <- c("","NA","0","Undetected","Not Detected",
                   "Field Parameters to be filled in", 
                   "Inhibited-to be re-ran", "#DIV/0!","-","In progress")

LIMSFullDF <- read_excel(LIMSFN,
                      na  =  missing_codes,
                      col_types = c(rep("guess",48),"text",rep("guess",12)))%>%
  rename(Site=wwtp_name,FlowRate=average_flow_rate,
         Cov1_below_lod=avg_sars_cov2_below_lod,cov2_conc=avg_sars_cov2_conc,
         BCoV=bcov_rec_rate,BCoVConc=bcov_spike_conc,county=county_names,
         Date=unformatted_collectdate,
         N1=n1_sars_cov2_conc,
         N1Error=n1_sars_cov2_error,
         N2=n2_sars_cov2_conc,
         N2Error=n2_sars_cov2_error,
         PMMoV=pmmov_conc,
         Pop=population_served)%>%
  mutate(Date=as.Date(Date),N1=as.numeric(N1),N2=as.numeric(N2),Pop=as.numeric(Pop),
         N1Error=as.numeric(N1Error),N2Error=as.numeric(N2Error),PMMoV=as.numeric(PMMoV),
         BCoV=as.numeric(BCoV),FlowRate=as.numeric(FlowRate))%>%
  select(Date,Site, BCoV, N1,N1Error,N2, N2Error,PMMoV,Pop,FlowRate)%>%
  mutate(Site=ifelse(Site=="Madison Metro","Madison",Site),
         Site=ifelse(Site=="Covid Sewage UW DORM","UW-LakeShore",Site),
         Site=ifelse(Site=="Covid Sewage UW Sell","UW-Sellery",Site),
         Site=ifelse(Site=="Madison-P2-Central","MMSD-P2",Site),
         Site=ifelse(Site=="Madison-P7-SE","MMSD-P7",Site),
         Site=ifelse(Site=="Madison-P8-West","MMSD-P8",Site),
         Site=ifelse(Site=="Madison-P11-SW","MMSD-P11",Site),
         Site=ifelse(Site=="Madison-P18-NE","MMSD-P18",Site),
         AVG = tmpfn(N1, N2),
         wt = 2 - is.na(N1) - is.na(N2))






#Reads Transformed HFG Data
print("HFGFrame")
HFGFrame <- HFGInfo(HFGWasteFN)%>%
  select(Date,Site=Plant,Filter,Well,N1=N1GC,N2=N2GC,PMMoV=PMMOVGC,Pct_BCoV=BCoV,AVG)%>%
  mutate(Filter=as.character(Filter),Well=as.character(Well))%>%
  rename(`Filter replicates`=Filter)
print("HFGFrame")



HFGCaseDFNoReported <- HFGCasesPARSER(HFGCaseFN)

#Reported Cases match census level case data
HFGCaseDFReportedOnly <- HFGCaseDFNoReported%>%
  mutate(Date=Date+1,ReportedCases=ConfirmedCases)%>%
  select(Date,Site,ReportedCases)

HFGCaseDF  <-  full_join(HFGCaseDFReportedOnly,HFGCaseDFNoReported,by=c("Date","Site"))


HFGCaseDFRoll <- HFGCaseDF%>%
  mutate(EpisodeCases=ifelse(EpisodeCases==-999,2.5,EpisodeCases))%>%
  mutate(CollectedCases=ifelse(CollectedCases==-999,2.5,CollectedCases))%>%
  mutate(ConfirmedCases=ifelse(ConfirmedCases==-999,2.5,ConfirmedCases))%>%
  RollAvg(Facet="Site")%>%
  select(Date,Site,ends_with("Cases"))

HFGPop <- read.csv("../../../UntrackedData/HFGPop.csv.txt")%>%
  select(Site=wwtp_name,Population=pop_served)%>%
  mutate(Population=Population/100000)

HFGCaseDF <- full_join(HFGPop,HFGCaseDF,by="Site")

HFGCaseDFRoll <- full_join(HFGPop,HFGCaseDFRoll,by="Site")

#Generating the weekends starts and end dates
#TO DO:Capture the weekend if the data intersects with it
HFGDateRangeDF <- WeekendGen(HFGCaseDF$Date)


