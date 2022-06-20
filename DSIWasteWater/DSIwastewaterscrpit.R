library(devtools)

document()
build_vignettes()
dir.create("inst/doc", recursive = TRUE)
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)
build(vignettes = FALSE)
install(quick=FALSE)#build_vignettes = TRUE)

check(vignettes  = FALSE)
vignette("vignettes_DHSTopLevelAnalysis_Outlier")



devtools::install_github(
  "AFIDSI/COVID-19_WastewaterAnalysis",
  ref="MainBranchInPackageFormat", subdir = "DSIWasteWater",
  auth_token = "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",build_vignettes = TRUE,force = TRUE
)