InnerFunc <- function(DF,k,n,Formula){
  ww.x.subset = DF[c(k:(k+n)),]
  
  lm.subset = lm(Formula, # date included works same as days_since_last_sample
                 
                 data = ww.x.subset)
  
  summary(lm.subset)
  
  
  
  # Extract row to bind with workset
  
  ww.x.tobind = ww.x.subset %>%
    
    filter(date == max(date)) %>%
    
    select(WWTP, date) %>%
    
    mutate(days_elapsed = as.numeric(max(ww.x.subset$date) - min(ww.x.subset$date)),
           
           lmreg_n = nrow(ww.x.subset),
           
           lmreg_slope = summary(lm.subset)$coefficients[2,1],
           
           lmreg_sig = summary(lm.subset)$coefficients[2,n],
           
           modeled_percentchange = (10^(lmreg_slope*days_elapsed)-1)*100
    )
  return(ww.x.tobind)
}

HeatMapPlot <- function(DF,Path){
  Dim = ceiling(length(unique(DF$WWTP))/4)
  Gplt <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-2,xmax=date+2,
                  ymin=0,
                  ymax = 10,fill = cut(lmreg_slope,quantile(lmreg_slope),labels=FALSE)))+
    facet_wrap(~WWTP, scales = "free",ncol=4)
  
  ggsave(Path,plot=Gplt,
         width = 64,height=5*Dim,units="cm")
}