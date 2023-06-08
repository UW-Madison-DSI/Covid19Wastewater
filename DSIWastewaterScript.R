library(devtools)

Move_struct_R <- function(start, add_context = TRUE){
  to_move_files <- list.files(path = start, recursive = TRUE)
  for(file in to_move_files){
    file_type = strsplit(file, "/")[[1]]
    if(add_context){
      location = paste0("R/", start)
      for(sub_folder in file_type){
        location = paste0(location, "--", sub_folder)
      }
      file.copy(from = paste0(start,"/",file), 
                to = location)
    }else{
      file.copy(from = paste0(start,"/",file), "R")
    }
  }
}

QuickUpdate <- function(){
  unlink("R", recursive = T, force = T)
  dir.create("R")
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Move_struct_R("Analysis_tools")
  Move_struct_R("Data_Prep")
  Move_struct_R("Meta", add_context = FALSE)
  
  document()
  build(path = ".", vignettes = FALSE)
  #devtools::install_github("AFIDSI/DSIWastewater")
  install(quick=FALSE)
}

LongUpdate <- function(){
  unlink("R", recursive = T, force = T)
  dir.create("R")
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  Move_struct_R("Analysis_tools")
  Move_struct_R("Data_Prep")
  Move_struct_R("Meta", add_context = FALSE)
  
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
