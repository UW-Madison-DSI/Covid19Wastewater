## ----load packages------------------------------------------------------------
library(DSIWastewater)

## ----Variant data, eval = FALSE-----------------------------------------------
#  head(Covariants_data)

## ----Variant data 2, eval = FALSE---------------------------------------------
#  data(Covariants_data, package = "DSIWastewater")
#  
#  Covariants_data$category <- row.names(Covariants_data)
#  onlycovar <- Covariants_data[-c(1,2)]
#  mdfr <- melt(onlycovar, id.vars = "category")
#  
#  VariantPercentage <- ggplot(mdfr, aes(factor(category,levels = c(1:69)), value, fill = variable)) +
#      geom_bar(position = "fill", stat = "identity") +
#      scale_y_continuous(labels = percent) +
#      xlab("bi-weekly (2020-08-17 to 2022-12-05)") +
#      ylab("Covariant Percent")
#  
#  VariantPercentage
#  
#  #Run this for interactive graph
#  #ggplotly(VariantPercentage)

## ----Variant data 3, eval = FALSE---------------------------------------------
#  percentages <- mdfr %>%
#    group_by(category) %>%
#    mutate(sum = sum(value), percent = value/sum, majority = case_when(percent > .5 ~ paste(variable)))
#  
#  per <- percentages %>% drop_na(majority)
#  
#  dates <- Covariants_data[c("week","category")]
#  
#  perDates <- merge(per,dates,by="category")
#  
#  ggplot(perDates, aes(x=week, y=percent,color=majority)) +
#    geom_point() +
#    xlab("bi-weekly") +
#    ylab("Percent of total cases (above 50%)") +
#    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#  
#  
#  

