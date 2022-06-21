library(devtools)

document()
build_vignettes()
dir.create("inst/doc", recursive = TRUE)
file.copy(dir("doc", full.names=TRUE), "inst/doc", overwrite=TRUE)
build(vignettes = FALSE)
install(quick=FALSE)

check(vignettes  = FALSE)
vignette("vignettes_DHSTopLevelAnalysis_Outlier")
vignette("vignettes_DHSTopLevelAnalysis_Base")
