## ----load packages------------------------------------------------------------
library(DSIWastewater)
library(ggplot2)

## ----Wastewater Data----------------------------------------------------------
data(WasteWater_data, package = "DSIWastewater")
ggplot() +
  geom_point(data = WasteWater_data, aes(x = date, y = N1), color = "blue") +
  geom_point(data = WasteWater_data, aes(x = date, y = N2), color = "red") + 
  scale_y_log10()
head(WasteWater_data)

## ----case data----------------------------------------------------------------
data(Case_data, package = "DSIWastewater")
ggplot() +
  geom_point(data = Case_data, aes(x = date, y = conf_case), color = "blue") +
  scale_y_log10()
head(Case_data)

## ----HFG Wastewater Data------------------------------------------------------
data(HFGWaste_data, package = "DSIWastewater")
ggplot() +
  geom_point(data = HFGWaste_data, aes(x = date, y = N1), color = "blue") +
  geom_point(data = HFGWaste_data, aes(x = date, y = N2), color = "red") + 
  scale_y_log10()
head(HFGWaste_data)

## ----HFG Case data------------------------------------------------------------
data(HFGCase_data, package = "DSIWastewater")
ggplot() +
  geom_point(data = HFGCase_data, aes(x = date, y = ConfirmedCases), color = "blue") +
  scale_y_log10()
head(HFGCase_data)

## ----Population data----------------------------------------------------------
data(pop_data, package = "DSIWastewater")
head(pop_data)

