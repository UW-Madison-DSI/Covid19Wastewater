## ----load packages------------------------------------------------------------
library(DSIWastewater)

## ----Example Data-------------------------------------------------------------
data(Example_data, package = "DSIWastewater")
head(Example_data)

## -----------------------------------------------------------------------------
ggplot(Example_data, aes(x=date, y=geo_mean, color = site)) +
  geom_point()

