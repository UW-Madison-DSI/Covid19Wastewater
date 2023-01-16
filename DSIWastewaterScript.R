library(devtools)

Move_struct_R <- function(start){
  unlink("R", recursive = T, force = T)
  dir.create("R")
  to_move_files <- list.files(path = start, recursive = TRUE)
  for(file in to_move_files){
    file_type = strsplit(file, "/")[[1]]
    if(file_type[1] == "meta"){
      file.copy(from = paste0(start,"/",file), "R")
    }else{
      file.copy(from = paste0(start,"/",file), 
                to = paste0("R/",file_type[1],"--",file_type[2]))
    }
  }
}

QuickUpdate <- function(){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Move_struct_R("struct_R")
  document()
  build(path = ".", vignettes = FALSE)
  install(quick=FALSE)
  unlink("R", recursive = T, force = T)
}

LongUpdate <- function(){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Move_struct_R("struct_R")
  build_vignettes(quiet=FALSE)
  dir.create("inst/doc", recursive = TRUE)
  file.copy(dir("doc", full.names=TRUE), "inst/doc", overwrite=TRUE)
  file.remove("DSIWastewater_0.2.01.tar.gz")
  QuickUpdate()
  check(args = c("--no-tests"), vignettes = FALSE)
  test()
  unlink("R/*", recursive = T, force = T)
}

QuickUpdate()

#Move_struct_R(struct_R)
#vignette(package = "DSIWastewater")
#vignette("vignettes_DHSTopLevelAnalysis_Outlier")

