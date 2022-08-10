library(devtools)
document()
build(path = ".", vignettes = FALSE)
install(quick=FALSE)



document()
build_vignettes(quiet=FALSE)
dir.create("inst/doc", recursive = TRUE)
file.copy(dir("doc", full.names=TRUE), "inst/doc", overwrite=TRUE)
file.remove("DSIWastewater_0.2.01.tar.gz")
build(path = ".", vignettes = FALSE, args = c("--resave-data"))
install(quick=FALSE)
check(args = c("--no-tests"), vignettes = FALSE)
test()
vignette(package = "DSIWastewater")
vignette("vignettes_DHSTopLevelAnalysis_Outlier")