library(devtools)

document()
build_vignettes()
build(vignettes = FALSE)
install()

