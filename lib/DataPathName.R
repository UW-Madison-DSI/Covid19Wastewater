HFGWastePath = function(BaseDir){
  PathName = paste0(BaseDir,"data/processed/HFGWasteData011422.csv")
  return(PathName)
}

HFGCasePath = function(BaseDir){
  PathName = paste0(BaseDir,"data/processed/HFGCaseData_2021-05-07.csv")
  return(PathName)
}

LIMSWastePath = function(BaseDir){
  PathName = paste0(BaseDir,"data/processed/DHSWasteData-4_21_2022.csv")
  return(PathName)
}#LIMSWasteData_02-09-22.csv

LIMSCasePath = function(BaseDir){
  PathName = paste0(BaseDir,"data/processed/DHSCaseData-4_21_2022.csv")
  return(PathName)
}#MMSD_Interceptor_Cases_2_7_22.csv

RmdKnitPar = function(input, ...){
  
  rmarkdown::render(
    input,
    output_file = paste0("RmDOutput/",
                         xfun::sans_ext(input), '-', Sys.Date(), '.',
                         xfun::file_ext(input)
    ),
    envir = globalenv()
  )
}