## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, Warning = FALSE, message = FALSE)
options(warn=-1)

## ----load data----------------------------------------------------------------
library(DSIWastewater)
library(dplyr)
library(ggplot2)

data("Example_data", package = "DSIWastewater")

smoothing_df <- Example_data%>%
  select(site, date, N1, N2)%>%
  filter(N1 != 0, N2 != 0)%>%
  mutate(N1 = log(N1), N2 = log(N2))

base_plot <- smoothing_df%>%
  ggplot(aes(x = date))+
  geom_point(aes(y = N1, color = "N1"))+
  facet_wrap(~site)

base_plot

## ----sgolaySmoothMod----------------------------------------------------------
# Example code demonstrating sgolaySmoothMod() function
sgolay_smooth <- DSIWastewater:::sgolaySmoothMod(smoothing_df, "N1", "N1_sgolay", poly=5,n="guess", Filter = NULL)

sgolay_plot <- base_plot + 
  geom_line(aes(y = N1_sgolay, color = "default sgolay"), data = sgolay_smooth, linewidth = 1.25)

param_sgolay_smooth <- DSIWastewater:::sgolaySmoothMod(sgolay_smooth, "N1", "N1_sgolay_2", poly=2, n = "guess", Filter = NULL)

alt_plot <- sgolay_plot + 
  geom_line(aes(y = N1_sgolay_2, color = "poly 1 sgolay"), data = param_sgolay_smooth, linewidth = 1.25)


param_sgolay_smooth_2 <- DSIWastewater:::sgolaySmoothMod(sgolay_smooth, "N1", "N1_sgolay_3", poly = 5, n = 31, Filter = NULL)


show_plot <- alt_plot + 
  geom_line(aes(y = N1_sgolay_3, color = "n 20 sgolay"), data = param_sgolay_smooth_2, linewidth = 1.25)

show_plot+
  ggtitle("Savitzky-Golay smoothing with diffrent parameters")

## ----loessSmoothMod-----------------------------------------------------------
# Example code demonstrating loessSmoothMod() function
loess_smooth <- loessSmoothMod(sgolay_smooth, "N1", "N1_loess", span="guess", Filter = NULL)

all_smooth_plot <- sgolay_plot + geom_line(aes(y = N1_loess, color = "default loess"), data = loess_smooth, size = 1.25)

loess_plot <- base_plot + geom_line(aes(y = N1_loess, color = "default loess"), data = loess_smooth, size = 1.25)

param_loess_smooth <- loessSmoothMod(loess_smooth, "N1", "N1_loess_2", span = .1, Filter = NULL)

alt_plot <- loess_plot + geom_line(aes(y = N1_loess_2, color = "poly 1 loess"), data = param_loess_smooth, size = 1.25)


param_loess_smooth_2 <- loessSmoothMod(loess_smooth, "N1", "N1_loess_3", span= .2, Filter = NULL)


show_plot <- alt_plot + geom_line(aes(y = N1_loess_3, color = "n 20 loess"), data = param_loess_smooth_2, size = 1.25)

show_plot+
  ggtitle("loess smoothing with diffrent parameters")

## ----expsmoothMod, include = FALSE--------------------------------------------
# Example code demonstrating expSmoothMod() function
exp_smooth <- DSIWastewater:::expSmoothMod(loess_smooth, "N1", "N1_exp", Filter = NULL)

all_smooth_plot <- all_smooth_plot + 
  geom_line(aes(y = N1_exp, color = "default exp"), data = exp_smooth, size = 1.25)

exp_plot <- base_plot + 
  geom_line(aes(y = N1_exp, color = "default exp"), data = exp_smooth, size = 1.25)

param_exp_smooth <- DSIWastewater:::expSmoothMod(exp_smooth, "N1", "N1_exp_2", alpha = .3, Filter = NULL)

alt_plot <- exp_plot + geom_line(aes(y = N1_exp_2, color = "exp alpha .01"), data = param_exp_smooth, size = 1.25)


param_exp_smooth_2 <- DSIWastewater:::expSmoothMod(exp_smooth, "N1", "N1_exp_3", beta = .05, Filter = NULL)


show_plot <- alt_plot + geom_line(aes(y = N1_exp_3, color = "exp beta .005"), data = param_exp_smooth_2, size = 1.25)

show_plot+
  ggtitle("exp smoothing with diffrent parameters")

## ----final compare------------------------------------------------------------
all_smooth_plot

