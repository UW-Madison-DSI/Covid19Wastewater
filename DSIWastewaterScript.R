
library(devtools)

Move_struct_R <- function(start, end = "R", add_context = TRUE){
  to_move_files <- list.files(path = start, recursive = TRUE)
  for(file in to_move_files){
    is_dead = FALSE
    file_type = strsplit(file, "/")[[1]]
    if(add_context){
      location = paste0(end, "/", start)
      for(sub_folder in file_type){
        location = paste0(location, "--", sub_folder)
        if(sub_folder == "README"){
          print(location)
          is_dead = TRUE
        }
      }
      if(!is_dead){
        file.copy(from = paste0(start,"/",file), to = location)
      }else{
        print(location)
      }
    }else{
      file.copy(from = paste0(start,"/",file), end)
    }
  }
}

package_update <- function(path = ".", update_examples = F, update_test = F){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  #setwd(path)
  #file.remove("DSIWastewater_0.5.1.tar.gz")
  unlink("R", recursive = T, force = T)
  dir.create("R")
  Move_struct_R("src", "R")
  Move_struct_R("meta", "R", add_context = FALSE)
  
  if(update_examples){
    unlink("vignettes", recursive = T, force = T)
    dir.create("vignettes")
    Move_struct_R("examples", "vignettes", add_context = FALSE)
    #build_vignettes(quiet=FALSE)
    dir.create("inst/doc", recursive = TRUE)
    file.copy(dir("doc", full.names=TRUE), "inst/doc", overwrite=TRUE)
  }
  document()
  build(path = ".", vignettes = FALSE)
  #devtools::install_github("AFIDSI/DSIWastewater")
  if(update_examples){
    unlink("inst/doc", recursive = T, force = T)
  }
  if(update_test){
    check(args = c("--no-tests"), vignettes = FALSE)
    test()
  }
  #install(quick=TRUE)
}


package_update(update_examples = T, update_test = F)