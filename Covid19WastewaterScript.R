
library(devtools)

mach_reg <- function(file_path, match, split){
  file_break = strsplit(file_path, split)[[1]][2]
  return(file_break == match)
}

move_struct_R <- function(start, end = "R", add_context = TRUE, end_in = NA){
  to_move_files <- list.files(path = start, recursive = TRUE)
  
  for(file in to_move_files){
    is_dead = FALSE
    
    if(!is.na(end_in)){
      is_dead = !mach_reg(file, end_in, "\\.")
    }
    
    file_type = strsplit(file, "/")[[1]]
    if(add_context){
      location = paste0(end, "/", start)
      for(sub_folder in file_type){
        location = paste0(location, "--", sub_folder)
        is_dead = sub_folder == "README"
        
      }
      if(!is_dead){
        file.copy(from = paste0(start,"/",file), to = location)
      }
    }else{
      if(!is_dead){
        file.copy(from = paste0(start,"/",file), end)
      }
    }
  }
}

build_vignette_DSI <- function(){
  build_vignettes(quiet = F)
  unlink("docs/vignettes", recursive = T, force = T)
  dir.create("docs/vignettes")
  for(fileName in dir("doc")){
    if(!mach_reg(fileName, "Rmd", "\\.") && !mach_reg(fileName, "R", "\\.")){
      file.copy(paste0("doc/", fileName), paste0("docs/vignettes/", fileName), overwrite=TRUE)
    }
  }
}


package_update <- function(path = ".", update_examples = F, update_test = F){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  #setwd(path)
  #file.remove("Covid19Wastewater_0.5.1.tar.gz")
  #Delete R folder and all .R folders in it
  #file.remove("NAMESPACE")
  unlink("R", recursive = T, force = T)
  dir.create("R")
  move_struct_R("library", "R")
  move_struct_R("meta_info", "R", add_context = FALSE)
  
  unlink("vignettes", recursive = T, force = T)
  dir.create("vignettes")
  move_struct_R("examples", "vignettes", add_context = FALSE, end_in = "Rmd")
  if(update_examples){#build vignette
    build_vignette_DSI()
  }
  
  document()
  build(path = ".", vignettes = update_examples)
  #devtools::install_github("UW-Madison-DSI/Covid19Wastewater")
  
  if(update_test){
    check(args = c("--no-tests"), vignettes = FALSE)
    test()
  }
  #check(vignettes = FALSE)
  install(quick=T, build = T, build_vignettes = update_examples, force = F)
}
package_update(update_examples = T, update_test = F)
3
#load_all()
#run_examples( fresh = F)#, start = "factorVecByNumPoints")#DF_date_vector, 
check(vignettes = T)

#install(quick=T, build = T, build_vignettes = T, force = F)
#devtools::check_rhub()
  