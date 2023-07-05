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

## ----Variant data, eval = FALSE-----------------------------------------------
#  data(Covariants_data, package = "DSIWastewater")
#  Covariants_data$category <- row.names(Covariants_data)
#  onlycovar <- Covariants_data[-c(1,2)]
#  mdfr <- melt(onlycovar, id.vars = "category")
#  
#  p <- ggplot(mdfr, aes(factor(category,levels = c(1:69)), value, fill = variable)) +
#      geom_bar(position = "fill", stat = "identity") +
#      scale_y_continuous(labels = percent) +
#      xlab("Week") +
#      ylab("Covariant Percent")
#  
#  ggplotly(p)
#  
#  
#  percentages <- mdfr %>%
#    group_by(category) %>%
#    mutate(sum = sum(value), percent = value/sum, majority = case_when(percent > .5 ~ paste(variable)))
#  
#  per <- percentages %>% drop_na(majority)
#  
#  
#  p2 <- ggplot(per, aes(x=category, y=percent,color=majority)) +
#    geom_point()
#  
#    ggplotly(p2)
#  head(Covariants_data)

## ----Interceptor Case data----------------------------------------------------
#InterceptorCase_data  
data(InterceptorCase_data, package = "DSIWastewater")
ggplot() +
  geom_point(data = InterceptorCase_data, aes(x = Date, y = FirstConfirmed), color = "blue") +
  scale_y_log10()
head(InterceptorCase_data)

## ----Example Data-------------------------------------------------------------
data(Example_data, package = "DSIWastewater")
head(Example_data)

