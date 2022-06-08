MainCaseDataPrep <- function(Dir,SiteC){
  DF <- ParseData(LIMSCasePath(Dir))%>% 
    rename(Cases = FirstConfirmed)%>%
    mutate(Cases = ifelse(is.na(Cases),0,Cases))
  if(SiteC!=""){
    DF2 <- DF%>%
      filter(Site %in% SiteC)
  }else{
    DF2 <- DF
  }
  DF2 <- DF2%>%
    group_by(Site)%>%
    arrange(Date)%>%
    mutate(SevenDayMACases = rollapply(data = Cases, width = 14, FUN = mean, 
                                       partial = TRUE))%>%
    select(Date, Site, Cases,SevenDayMACases)
  return(DF2)
}

MainWastePrep <- function(Dir,SiteC){
  DF <- ParseData(LIMSWastePath(Dir))
  if(SiteC!=""){
    DF <- DF%>%
      filter(Site %in% SiteC)
  }
  
  DF <- DF%>%
    mutate(N1Pure = ifelse(is.na(N1),N2,N1),
           N2Pure = ifelse(is.na(N2),N1,N2),
           GeoMeanN12 = exp((log(N1Pure)+log(N2Pure))/2),
           N1FlowPop = Pop*N1/FlowRate)%>%
    select(Date, Site, N1 , N2 , GeoMeanN12, N1FlowPop,Pop,N1LOD)
  return(DF)
}


SLDSmoothMod <- function(DF, Width){
  Mean <- 11.73
  StandardDeviation <- 7.68
  Scale = StandardDeviation^2/Mean
  Shape = Mean/Scale

  weights <- dgamma(1:Width, scale = Scale, shape = Shape)
  ModDF <- DF%>%
    arrange(Date)%>%
    mutate(
      SLDCases = c(rep(NA,Width-1), #elimination of starting values not relevant
                   #as we have a 50+ day buffer of case data
                   rollapply(Cases,width=Width,FUN=weighted.mean,
                             w=weights,
                             na.rm = TRUE)
                   #,rep(NA,10)
      ))#no missing data to remove
  return(ModDF)
}

MinMaxSiteFixing <- function(Vec, Bar){
  
  NormVec = MinMaxNormalization(Vec,MinMaxCollector(SubVec))
  BarRange = MinMaxCollector(Bar)
  RefitVec = NormVec*(BarRange[2]-BarRange[1])+BarRange[1]
  return(RefitVec)
}



NormWithVec <- function(Vec1, Levels){
  
  return(((Vec1-Levels[1])/
                     (Levels[2]-Levels[1]))*
                      (Levels[4]-Levels[3])+
                      Levels[3])
}


MinMaxCalc <- function(Vec){
  return(c(min(Vec,na.rm=TRUE),max(Vec,na.rm=TRUE)))
}

QuintCalc <- function(Vec,low,high){
  return(c(quantile(Vec,low,na.rm=TRUE),quantile(Vec,high,na.rm=TRUE)))
}



NormOP <- function(DF,Vec1Name,RetName,Levels){
  Vec1 <- pull(DF,!!Vec1Name)
  RetDF <- DF
  RetDF[[RetName]] <- NormWithVec(Vec1,Levels)
  return(RetDF)
}

NormMinMax <- function(DF,Vec1Name,Vec2Name,RetName){
  Vec1 <- pull(DF,!!Vec1Name)
  Vec2 <- pull(DF,!!Vec2Name)
  Levels <- c(MinMaxCalc(Vec1),MinMaxCalc(Vec2))
  return(NormOP(DF,Vec1Name,RetName,Levels))
}

NormQuint <- function(DF,Vec1Name,Vec2Name,RetName){
  Vec1 <- pull(DF,!!Vec1Name)
  Vec2 <- pull(DF,!!Vec2Name)
  Levels <- c(QuintCalc(Vec1,.1,.9),QuintCalc(Vec2,.1,.9))
  return(NormOP(DF,Vec1Name,RetName,Levels))
}

NormThird <- function(DF,Vec1Name,Vec2Name, Vec3Name,RetName){
  Vec3 <- pull(DF,!!Vec3Name)
  Vec2 <- pull(DF,!!Vec2Name)
  Levels <- c(QuintCalc(Vec3,.1,.9),QuintCalc(Vec2,.1,.9))
  return(NormOP(DF,Vec1Name,RetName,Levels))
}


#' Title
#'
#' @param DF 
#' @param FacVar 
#' @param FiltVar 
#'
#' @return
#' @export
#'
#' @examples
FactorVecByNumPoints <- function(DF,FacVar, FiltVar){
  FactorOrder <- (DF%>%
                    filter(!is.na(!!sym(FiltVar)))%>%
                    group_by(!!sym(FacVar))%>%
                    summarise(n=n())%>%
                    arrange(desc(n)))[[FacVar]]
  
  
  FacedDF <- DF%>%
    mutate(!!FacVar := factor(!!sym(FacVar),FactorOrder))
  
  return(FacedDF)
}



Find_Max_CCF<- function(a,b)
{
  d <- ccf(a, b, plot = FALSE, na.action = na.pass)
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_max = res[which.max(res$cor),]
  return(res_max)
}

MaxCFDFGen <- function(DF, arg1, arg2)
{
  retDF <- Find_Max_CCF(DF[[arg1]], DF[[arg2]])%>%
    mutate(Site = unique(DF$Site)[1], 
           yPos = (max(DF[[args$WasteVar]],na.rm=TRUE) - min(DF[[args$WasteVar]],na.rm=TRUE)))
  return(retDF)
}