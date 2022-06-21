library(devtools)

document()
build_vignettes()
dir.create("inst/doc", recursive = TRUE)
file.copy(dir("doc", full.names=TRUE), "inst/doc", overwrite=TRUE)
build(vignettes = FALSE)
install(quick=FALSE)
devtools::test()#VerySlow
check(vignettes  = FALSE)
vignette(package = "DSIWasteWater")
