## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, Warning = FALSE, message = FALSE)
options(warn=-1)

## ----load and show data-------------------------------------------------------
#add start and end date?
library(DSIWastewater)
library(dplyr)
library(ggplot2)

data(WasteWater_data, package = "DSIWastewater")

## ----explain all  most relavent columns, warning = FALSE----------------------
#could put in the work to explain all 62 columns but it should be fine for now
WasteWater_data%>%
  filter(site == "Madison")%>%
  ggplot(aes(x = date), size = .3)+
  geom_point(aes(y = N1, color = "N1"))+
  geom_point(aes(y = N2, color = "N2"))+
  scale_y_log10()

## ----explain  show use case and link to other vignettes-----------------------
head(WasteWater_data)

