OutLier_pointPlot_Prep <- function(DF, OutlierFlagMain, 
                                   OutlierFlagSub, MainComp, SubComp){
  ReturnDF <- DF%>%
    mutate(
      !!paste0("Outlier",MainComp) := ifelse(!!sym(OutlierFlagMain), !!sym(MainComp),NA),
      !!paste0("Outlier",SubComp) := ifelse(!!sym(OutlierFlagSub), !!sym(SubComp),NA),
      !!MainComp := ifelse(!(!!sym(OutlierFlagMain)), !!sym(MainComp),NA),
      !!SubComp := ifelse(!(!!sym(OutlierFlagSub)), !!sym(SubComp),NA)
      )
  return(ReturnDF)
}

MassBalence_BarPlot_Prep <- function(DF, Break, SepBreak, MainComp){
  MainDF <- DF %>%
    filter(!(!!sym(Break) %in% SepBreak),
           !is.na(!!sym(MainComp)))
  
  SubDF <- DF %>%
      filter(Site %in% SepBreak,
             !is.na(!!sym(MainComp)),
             Date %in% MainDF[["Date"]])%>%
      mutate(!!paste0("Selected",Break) := !!sym(MainComp),
             !!MainComp := NA)
    
    ReturnDF <- rbind(MainDF,SubDF)
    return(ReturnDF)
}