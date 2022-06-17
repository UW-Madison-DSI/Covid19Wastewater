library(devtools)

document()
build_vignettes()
build(vignettes = TRUE)
install()#build_vignettes = TRUE)

check(vignettes  = FALSE)
