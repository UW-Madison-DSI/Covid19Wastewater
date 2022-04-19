DoRanking <- function(DF,IsGrouped,isFiltered){
  UsedDF <- DF%>%
    filter(!is.na(N1),!is.na(N2))
  if(IsGrouped){
    UsedDF <- UsedDF%>%
      group_by(Site)
  }
  if(isFiltered){
    UsedDF <- UsedDF%>%
      filter(Date<mdy("10/31/2021"))
  }
  RetDF <- UsedDF%>%
    arrange(Date)%>%
    mutate(N1 = ifelse(!is.na(N1),N1,0))%>%
    mutate(N2 = ifelse(!is.na(N2),N2,0))%>%
    mutate(N1RankLeft = rank(desc(N1 - lag(N1))),
           N1RankRight = rank(desc(N1 - lead(N1))),
           N2RankLeft = rank(desc(N2 - lag(N2))),
           N2RankRight = rank(desc(N2 - lead(N2))))%>%
    select(Date,Site,N1RankLeft,N1RankRight,N2RankLeft,N2RankRight,N1,N2,FlowRate)%>%
    mutate(MaxN1Rank = pmax(N1RankLeft,N1RankRight,N2RankLeft,N2RankRight))%>%
    select(Date,Site,MaxN1Rank,N1,N2,FlowRate)
  return(RetDF)
}

ThresholdingLabel <- function(X,ThresholdC){
  A = 0
  for (i in 1:length(ThresholdC)){
    A = A + (X<=ThresholdC[i])
    
  }
  return(factor(A))
}

ThresholdGenDF <- function(DF, SysUsed ,Thresholds){
  ThresholdFlagDF <- DF%>%
    filter(!is.na(!!sym(SysUsed)))%>%
    mutate(ThresholdOutlier= ThresholdingLabel(!!sym(SysUsed),Thresholds))%>%
    arrange(Date,Site)
  
  return(ThresholdFlagDF)
}

CreateDF <- function(csvPath){
  LIMSFullDF <- read.csv(csvPath)%>%
    mutate(Date=as.Date(Date,origin="1970-01-01 UTC"))%>%
    filter(Site %in% c("MMSD-P11","MMSD-P18","MMSD-P2","MMSD-P7","MMSD-P8"))#,"Madison"
  
  RankingDFgroupedEarlyDay <- DoRanking(LIMSFullDF,TRUE,TRUE)%>%
    mutate(Pop = case_when(
      Site=="MMSD-P2" ~ 111967,
      Site=="MMSD-P7" ~ 81977,
      Site=="MMSD-P8" ~ 127634,
      Site=="MMSD-P11" ~ 130799,
      Site=="MMSD-P18" ~ 151470,
      Site=="Madison" ~ 603847,
    ))%>%
    mutate(FlowN1 = FlowRate*N1,
           FlowN2 = FlowRate*N2,
           FlowPopN1 = FlowRate*N1/Pop,
           FlowPopN2 = FlowRate*N2/Pop)
  Thresh <- c(4, 11)
  UsedDF <- ThresholdGenDF(RankingDFgroupedEarlyDay, "MaxN1Rank", Thresh)%>%
    rename(Threshold = ThresholdOutlier)%>%
    mutate(Threshold = ifelse((Threshold)==1,"Permissive",
                              ifelse((Threshold==2),"Strict","Inliers")))
  

  return(UsedDF)
}


LoessGen <- function(Vec1,Vec2,spanIn){
  RetVec <- loessFit(y=(Vec1), 
           x=Vec2, #create loess fit of the data
           span=spanIn, #span of .2 seems to give the best result, not rigorously chosen
           iterations=2)$fitted#2 iterations remove some bad patterns
  return(RetVec)
}