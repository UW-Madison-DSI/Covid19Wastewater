HFGWastePath = function(BaseDir){
  PathName = paste0(BaseDir,"COVID-19_WastewaterAnalysis/data/processed/HFGWasteData011422.csv")
  return(PathName)
}

HFGCasePath = function(BaseDir){
  PathName = paste0(BaseDir,"COVID-19_WastewaterAnalysis/data/processed/HFGCaseData_2021-05-07.csv")
  return(PathName)
}

LIMSWastePath = function(BaseDir){
  PathName = paste0(BaseDir,"COVID-19_WastewaterAnalysis/data/processed/LIMSWasteData_01-25-21_01-05-22.csv")
  return(PathName)
}

LIMSCasePath = function(BaseDir){
  PathName = paste0(BaseDir,"COVID-19_WastewaterAnalysis/data/processed/MMSD_Cases_processed.csv")
  return(PathName)
}