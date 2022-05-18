FCVLM <- function(Formula, data){
  lenDF <- nrow(data)
  LMOptions <- LMSelectBestList(
    lapply(1:(lenDF+1), LMDropList, data, Formula))
}

LMDropList <- function(n, data, Formula){
  ReducedDF <- data[-n,] 
  return(lm(Formula, data = ReducedDF))
}

LMSelectBestList <- function(LMList, Verbose = FALSE){
  BestPVal <- 1
  RetLM <- LMList[[1]]
  for(i in 1:length(LMList)){
    ModPVal <- summary(LMList[[i]])$coefficients[2,4]
    if(is.nan(ModPVal)){next}
    if(Verbose){
      print(paste(BestPVal, ModPVal))
    }
    if(BestPVal > ModPVal){
      BestPVal <- ModPVal
      RetLM <- LMList[[i]]
    }
  }
  return(RetLM)
}


InnerFunc <- function(Formula, DF, k, n, robust = FALSE){
  ww.x.subset = DF[c(k:(k+n)),]
  
  if(robust){
    
    lm.subset = FCVLM(Formula, data = ww.x.subset)
    
  }else{
    
    lm.subset = lm(Formula, # date included works same as days_since_last_sample
                   
                   data = ww.x.subset)
    
  }

  
  # Extract row to bind with workset
  ww.x.tobind = ww.x.subset %>%
    
    filter(date == max(date)) %>%
    
    select(WWTP, date) %>%
    mutate(days_elapsed = as.numeric(max(ww.x.subset$date) - min(ww.x.subset$date)),
           
           lmreg_n = nrow(ww.x.subset),
           
           lmreg_slope = summary(lm.subset)$coefficients[2,1],
           
           lmreg_sig = summary(lm.subset)$coefficients[2,4],
           
           modeled_percentchange = ((10^(lmreg_slope*days_elapsed))-1)*100,
           
           Method = as.character(terms(lm.subset)[[2]]),
           
           LMmethod = robust
    )

  return(ww.x.tobind)
}

OuterLoop <- function(DF,Formulas,n = 4, PSigTest = TRUE,robust=FALSE, verbose = FALSE){
  reg_estimates = as.data.frame(matrix(ncol=9, nrow=0))
  
  colnames(reg_estimates) = c("WWTP", "date", "days_elapsed", "lmreg_n" , 
                              "lmreg_slope", "lmreg_sig", "modeled_percentchange", "Method", "LMmethod")
  
  distinct_wwtps = DF %>%
    group_by(WWTP)%>%
    #filter(n()>5)%>%
    distinct(WWTP)
  
  for (i in 1:nrow(distinct_wwtps)){
    
    if(verbose){
      print(paste(distinct_wwtps[i,1]))
    }
    
    ww.x = DF %>%
      
      filter(WWTP==paste(distinct_wwtps[i,1]))
    
    for (k in 1:(nrow(ww.x) - n)){
      
      if(robust){
        # Join with full set of reg estimates with robust LM
        ww.x.tobind = bind_rows(lapply(Formulas,InnerFunc,DF=ww.x,k=k,n=n+1, robust = TRUE))
      }else{
        #Join with full set of reg estimates
        ww.x.tobind = bind_rows(lapply(Formulas,InnerFunc,DF=ww.x,k=k,n=n))
      }
      reg_estimates = rbind(ww.x.tobind, reg_estimates)
    }
  }
  
  
  #Order WTTP by number of points
  reg_estimates <- FactorVecByNumPoints(reg_estimates,"WWTP","modeled_percentchange")
  
  
  
  #Bring into own function
  Catagorylabel = c("major decrease","moderate decrease","fluctuating ","moderate increase","major increase")
  
  reg_estimates <- reg_estimates%>%
    mutate(Catagory = cut(modeled_percentchange, c(-Inf,-50,-10,10,100,Inf),
                          include.lowest=TRUE,
                          ordered_result=TRUE))
           
  if(PSigTest){
    reg_estimates <- reg_estimates%>%
      mutate(Catagory = ifelse(lmreg_sig>.3, "no change", Catagory),
             Catagory = factor(Catagory, c(1,2,3,4,5,"no change"), 
                               labels = c( Catagorylabel, "no change"), exclude = NULL))
  }else{
    reg_estimates <- reg_estimates%>%
        mutate(Catagory = factor(Catagory, c(1,2,3,4,5), 
                               labels = c( Catagorylabel, "no change"), exclude = NULL))
  }
  return(reg_estimates)
}

CreateHeatMaps <- function(DF, ToMerge = FALSE){
  CatagoryColors <- c("#0571b0", "#92c5de", "#979797","#f4a582","#ca0020","Black")
  BarGridSmoothRaw <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-days_elapsed/2,xmax=date+days_elapsed/2,
                  ymin=0,
                  ymax = 10,fill = Catagory))+
    facet_grid (Method~WWTP)+
    scale_fill_manual(values = CatagoryColors)
  if(ToMerge){
    BarGridSmoothRaw <- BarGridSmoothRaw+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }

}


