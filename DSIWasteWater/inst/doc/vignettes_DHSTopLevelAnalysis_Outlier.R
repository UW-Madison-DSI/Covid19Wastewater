## ----load packages------------------------------------------------------------
library(DSIWasteWater)
library(dplyr)

## ----create worksheet4--------------------------------------------------------
data(Data_wastewater, package = "DSIWasteWater")

#TODO abstract saved data -> workset4? save workset4 instead?
#Filter number of sites for faster kniting time?

df <- Data_wastewater

## format data as DHS code expects
df <- df %>% 
  select(
    wwtp_name,sample_collect_date,population_served,  ## site data
    n1_sars_cov2_conc, n2_sars_cov2_conc,             ## N1, N2 measurement
    average_flow_rate                                 ## sample covariates
  ) %>% 
  rename(WWTP = wwtp_name, date = sample_collect_date) %>% 
  mutate(date = as.Date(date,format="%m/%d/%Y"))

### Note: Replacement small values with LOD/2 (as per 5/20/2022 discussion w/DHS)
##   in bin/cleanData.pl

## dependent regression variable: log of normalized average SARS-COV-2 level
workset4 <- df %>% 
  filter(average_flow_rate != "NA") %>% 
  mutate (geoMean = sqrt(n1_sars_cov2_conc*n2_sars_cov2_conc)) %>% 
  mutate(sars_cov2_adj_load_log10 = log10(
    geoMean*average_flow_rate/population_served    
  )
  )


## filter out sites with too few measurements
##  and sort by date;
workset4 <- workset4 %>% 
  group_by(WWTP) %>% 
  mutate(n = n()) %>% 
  arrange(date, .by_group = TRUE) %>% 
  ungroup %>% 
  filter(n >= 150)#To reduce knitting time

## ----flag outlier-------------------------------------------------------------

df <- computeJumps(workset4)
df.ranked <- rankJumps(df)
df.ranked.quantile <- computeRankQuantiles(df.ranked)
df.classied <- flagOutliers(df.ranked.quantile, 9)
df.created <- RemoveOutliers(df.classied)


## ----run package code---------------------------------------------------------

reg_estimates <- BuildRegressionEstimateTable(df.created, 
                                     c("sars_cov2_adj_load_log10",
                                     "sars_adj_log10_Filtered")
                                    )
head(reg_estimates)

## ----temp explore,fig.width=8-------------------------------------------------
library(ggplot2)

ConfMatrix(reg_estimates, "Catagory", "sars_cov2_adj_load_log10", "sars_adj_log10_Filtered")


reg_estimates%>%
  group_by(Method,Catagory)%>%
  summarise(n = n())%>%
  ggplot(aes(x=Catagory,y=n))+
           geom_col(aes(fill=Method),position = "dodge")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## ----make DHS plot, fig.height=5,fig.width=158--------------------------------
#TODO restructure so it can accept 2 point vars
#underlying function can already do this, just not the wrapper
DHSTopLevelPlots(reg_estimates, df.created, 
                 PointVal = c( "sars_cov2_adj_load_log10","sars_adj_log10_Filtered"))

