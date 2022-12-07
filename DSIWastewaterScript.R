library(devtools)
QuickUpdate <- function(){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  document()
  build(path = ".", vignettes = FALSE)
  install(quick=FALSE)
}
LongUpdate <- function(){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  build_vignettes(quiet=FALSE)
  dir.create("inst/doc", recursive = TRUE)
  file.copy(dir("doc", full.names=TRUE), "inst/doc", overwrite=TRUE)
  file.remove("DSIWastewater_0.2.01.tar.gz")
  QuickUpdate()
  check(args = c("--no-tests"), vignettes = FALSE)
  test()
}
QuickUpdate()
#vignette(package = "DSIWastewater")
#vignette("vignettes_DHSTopLevelAnalysis_Outlier")

