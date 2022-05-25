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


InnerFunc <- function(Formula, DF, k, n, LMMethod = lm){
  
  lm.subset <- LMMethod(Formula, data = DF)
  
  # Extract row to bind with workset
  ww.x.tobind <- DF %>%
    
    filter(date == max(date)) %>%
      select(WWTP, date) %>%
      mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
             
             lmreg_n = nrow(DF),
             
             lmreg_slope = summary(lm.subset)$coefficients[2,1],
             
             lmreg_sig = summary(lm.subset)$coefficients[2,4],
             
             modeled_percentchange = ((10^(lmreg_slope*days_elapsed))-1)*100,
             
             Method = as.character(terms(lm.subset)[[2]]))

  return(ww.x.tobind)
}

OuterLoop <- function(DF,Formulas,n = 4, PSigTest = TRUE, LMMethod=lm, verbose = FALSE){
  
  reg_estimates = as.data.frame(matrix(ncol=9, nrow=0))
  
  colnames(reg_estimates) = c("WWTP", "date", "days_elapsed", "lmreg_n" , 
                              "lmreg_slope", "lmreg_sig", "modeled_percentchange", "Method", "LMmethod")
    
    if(verbose){
      print(unique(DF$WWTP)[[1]])
    }
    
    for (k in 1:(nrow(DF) - n)){
      
        ww.x.subset = DF[c(k:(k+n)),]
        
      #try({
        ww.x.tobind = Formulas%>%
          lapply(InnerFunc,
                 DF=ww.x.subset,
                 LMMethod = LMMethod)%>%
          bind_rows()
      #})
        reg_estimates <- rbind(reg_estimates, ww.x.tobind)
    }
  
  #Bring into own function
  Catagorylabel = c("major decrease","moderate decrease",
                    "fluctuating ", "no change",
                    "moderate increase","major increase")
  
  reg_estimates <- reg_estimates%>%
    mutate(Catagory = cut(modeled_percentchange, c(-Inf,-50,-10,10,100,Inf),
                          include.lowest=TRUE,
                          ordered_result=TRUE))
           
  if(PSigTest){
    reg_estimates <- reg_estimates%>%
      mutate(Catagory = ifelse(lmreg_sig>.3, "no change", Catagory))
  }
  
  reg_estimates <- reg_estimates%>%
    mutate(Catagory = factor(Catagory, c(1,2,3,"no change",4,5), 
                               labels = c( Catagorylabel), exclude = NULL))
  return(reg_estimates)
}

CreateHeatMaps <- function(DF, ToMerge = FALSE){#, 
  CatagoryColors <- c("#0571b0","#92c5de", "#979797","#f4a582","#ca0020","WHITE")
  BarGridSmoothRaw <- DF%>%
    ggplot()+
    geom_rect(aes(xmin=date-days_elapsed/2,xmax=date+days_elapsed/2,
                  ymin=0,
                  ymax = 10,fill = Catagory))+
    facet_grid(Method ~ WWTP)+
    scale_fill_manual(values = CatagoryColors)
  if(ToMerge){
    BarGridSmoothRaw <- BarGridSmoothRaw+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
  }
}

