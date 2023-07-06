## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, Warning = FALSE, message = FALSE)
options(warn=-1)

## ----load and show data-------------------------------------------------------
library(DSIWastewater)
library(dplyr)
library(ggplot2)

data(HFGCase_data, package = "DSIWastewater")

## ----catagorical variables, warning = FALSE-----------------------------------

HFGCase_data%>%
  filter(!is.na(ReportedCases))%>%
  ggplot(aes(x = date), size = .5)+
  geom_point(aes(y = ReportedCases, color = "Reported Cases"))+
  geom_point(aes(y = EpisodeCases, color = "Episode Cases"))+
  geom_point(aes(y = CollectedCases, color = "Collected Cases"))+
  geom_point(aes(y = ConfirmedCases, color = "Confirmed Cases"))+
  scale_y_log10()+
  facet_wrap(~site)

## ----explain / show use case and link to other vignettes----------------------
head(HFGCase_data)


