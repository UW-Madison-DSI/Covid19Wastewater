## ----load packages------------------------------------------------------------
library(DSIWasteWater)
library(dplyr)

## ----create worksheet4--------------------------------------------------------
data(Data_wastewater, package = "DSIWasteWater")

workset4 <- BuildWorkSheet4(Data_wastewater)

#Only show Site with 150 measurements for vignette 
workset4 <- workset4%>% 
  filter(n >= 150)

## ----flag outlier-------------------------------------------------------------
VecModOn <- "sars_cov2_adj_load_log10"

df.created <- workset4%>%
    rename(Date=date, Site = WWTP)%>%
    group_by(Site)%>%
    group_split()%>%
    lapply(LoessSmoothMod, VecModOn, "Loess", "guess")%>%
    lapply(ExpSmoothMod, VecModOn, "EXP")%>%
    lapply(sgolaySmoothMod, VecModOn, "sgolay")%>%
    bind_rows()%>%
    rename(date=Date, WWTP = Site)


## ----run package code---------------------------------------------------------

reg_estimates <- BuildRegressionEstimateTable(df.created, 
                                     c("sars_cov2_adj_load_log10",
                                     "Loess",
                                     "EXP",
                                     "sgolay")
                                    )
head(reg_estimates)

## ----temp explore,fig.width=6-------------------------------------------------
library(ggplot2)




ConfMatrix(reg_estimates, "Catagory", "sars_cov2_adj_load_log10", "Loess")


reg_estimates%>%
  group_by(Method,Catagory)%>%
  summarise(n = n())%>%
  ggplot(aes(x=Catagory,y=n))+
           geom_col(aes(fill=Method),position = "dodge")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## ----make DHS plot, fig.height=10,fig.width=78--------------------------------



DHSTopLevelPlots(reg_estimates, df.created, 
                 PointVal = c( "sars_cov2_adj_load_log10"),
                 LineVal = c("Loess",
                                     "EXP",
                                     "sgolay"))

