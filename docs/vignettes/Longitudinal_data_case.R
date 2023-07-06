## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, Warning = FALSE, message = FALSE)
options(warn=-1)

## ----load and show data-------------------------------------------------------
library(DSIWastewater)
library(dplyr)
library(ggplot2)

data(Case_data, package = "DSIWastewater")

## ----explain most relavent columns--------------------------------------------
main_plot <- Case_data%>%
  filter(site == "Madison")%>%
  ggplot(aes(x = date), size = .3)+
  geom_point(aes(y = tests + 1, color = "tests"))+
  geom_point(aes(y = conf_case + 1, color = "confirmed case"))+
  scale_y_log10()

main_plot


## ----explain / show use case and link to other vignettes----------------------
head(Case_data)

