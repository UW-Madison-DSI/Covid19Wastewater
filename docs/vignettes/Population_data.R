## ----load packages------------------------------------------------------------
library(DSIWastewater)

## ----Population data----------------------------------------------------------
data(pop_data, package = "DSIWastewater")
head(pop_data)

## -----------------------------------------------------------------------------
populationtemp <- pop_data %>% mutate(popgroup = case_when(pop >= 100000 ~ "Large",
                                                         pop >= 10000  ~ "Medium",
                                                         .default = "Small"))
head(populationtemp)

