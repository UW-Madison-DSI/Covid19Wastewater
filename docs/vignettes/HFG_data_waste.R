## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, Warning = FALSE, message = FALSE)
options(warn=-1)

## ----load and show data-------------------------------------------------------
library(DSIWastewater)
library(dplyr)
library(ggplot2)

data(HFGWaste_data, package = "DSIWastewater")

## ----catagorical variables----------------------------------------------------
#fix titles
#fix overploting points and axis
#drop well category in plot
HFGWaste_data%>%
  ggplot(aes(x = date, y = N1, color = as.factor(Filter), shape = as.factor(Well)), size = .5)+
  geom_point()+
  scale_y_log10()+
  facet_wrap(~site)

## ----explain / show use case and link to other vignettes----------------------
#change to table form
#over explain is better then under explain
#have explanation as an appendix
head(HFGWaste_data)

