#' FCVLM
#'
#'fit n-1 models droping 1 data point
#'returns the model with the lowest p-val form among the,
#'
#' @param Formula LM model to be fit on robust data
#' @param data Contains the data needed for Formula to work
#'
#' @return the best of the lm models by p values
FCVLM <- function(Formula, data){
  lenDF <- nrow(data)
  LMOptions <- lapply(1:(lenDF+1), LMDropList, Formula, data)%>%
                  LMSelectBestList()
}

#' LMDropList
#' 
#' fits a lm model with the nth row of data removed
#'
#' @param n 
#' @param Formula LM model to be fit on robust data
#' @param data Contains the data needed for Formula to work
#'
#' @return
#' @export
#'
#' @examples
LMDropList <- function(n, Formula, data){
  ReducedDF <- data[-n,] 
  return(lm(Formula, data = ReducedDF))
}

#' LMSelectBestList
#' 
#'
#' @param LMList List of posible linear models
#' @param Verbose prints the row removed and the p value
#'
#' @return the LM with the lowest P-value
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


#' DHSInnerLoop
#' 
#' Runs a Linear regression on the data and returns it in a form to be merged
#' in the DHSOuterLoop function
#'
#' @param Formula LM model to be fit on DF
#' @param DF Contains the data needed for Formula to work
#' @param LMMethod Controls what Linear model is applied. 
#' intended options are lm and FCVLM
#'
#' @return a row of a DF containing the 
#' WWTP, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
DHSInnerLoop <- function(Formula, DF, LMMethod = lm){
  IndiVar <- as.character(Formula)[2]
  
  if(length(na.omit(pull(DF,IndiVar))) < 2){#The lm call will fail with 1 row
    reg_estimates = data.frame(#create an empty DF that won't cause issues for row_bind
                                  WWTP = character(),
                                  date = as.Date(character()), 
                                  days_elapsed = double(),
                                  lmreg_n = integer(),
                                  lmreg_slope = double(),
                                  lmreg_sig = double(),
                                  modeled_percentchange = double(),
                                  Method = character()
                                  )
    return(reg_estimates)
  }
  
  lm.subset.sum <- suppressWarnings(#data is sometimes functionary linear.
    summary(LMMethod(Formula, data = DF))
  )
  
  # Extract row to bind with workset
  ww.x.tobind <- DF %>%
    
    filter(date == max(date)) %>%
    
      select(WWTP, date) %>%
    
      mutate(days_elapsed = as.numeric(max(DF$date) - min(DF$date)),
             
             lmreg_n = nrow(DF),
             
             lmreg_slope = lm.subset.sum$coefficients[2,1],
             
             lmreg_sig = lm.subset.sum$coefficients[2,4],
             
             modeled_percentchange = ((10^(lmreg_slope*days_elapsed))-1)*100,
             
             Method = IndiVar)

  return(ww.x.tobind)
}

#' DHSOuterLoop
#'
#' The DHS model system. runs a LM on each each set of n consecutive measurements
#' and returns a summary of the information
#'
#'
#' @param DF Contains TS data should only contain one WWTP
#' @param Formula LM model to be fit on a subsection of the data
#' @param n number of rows to be in each LM regression
#' @param LMMethod Controls what Linear model is applied. 
#' intended options are lm and FCVLM
#' @param verbose prints what site we are in
#'
#' @return a row of a DF containing the 
#' WWTP, last date, timespan, number of rows, model slope and significance,
#' and predicted percent change, and what linear model was used
DHSOuterLoop <- function(DF,Formulas,n = 4,LMMethod=lm, verbose = FALSE){
  
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
          lapply(DHSInnerLoop,
                 DF=ww.x.subset,
                 LMMethod = LMMethod)%>%
          bind_rows()
      #})
        reg_estimates <- rbind(reg_estimates, ww.x.tobind)
    }
  
  return(reg_estimates)
}


#' DHSClassificationFunc
#' 
#' Adds the DHS Classification scheme to data created by DHSOuterLoop
#'
#' @param DF 
#' @param PSigTest Controls if we filter values with P-values>.3
#'
#' @return
DHSClassificationFunc <- function(DF, PSigTest=TRUE){

  
  RetDF <- DF%>%
    mutate(Catagory = cut(modeled_percentchange, c(-Inf,-50,-10,10,100,Inf),
                          ordered_result=TRUE),
           Catagory = as.numeric(Catagory))
  
  if(PSigTest){
    RetDF <- RetDF%>%
      mutate(Catagory = ifelse(lmreg_sig>.3, "no change", Catagory))
    levl <- c(1,2,3,"no change",4,5)
    Catagorylabel = c("major decrease", "moderate decrease",
                      "fluctuating", "no change",
                      "moderate increase", "major increase")
  }else{
    levl <- c(1,2,3,4,5)
    Catagorylabel = c("major decrease", "moderate decrease",
                      "fluctuating", 
                      "moderate increase", "major increase")
  }
  
  RetDF <- RetDF%>%
    mutate(Catagory = factor(Catagory, levels = levl, 
                             labels =  Catagorylabel))
  return(RetDF)
}


#' CreateHeatMaps
#' 
#' Creates graphic of model prediction for each method
#'
#' @param DF 
#' @param ToMerge if true we remove the lower labels
#'
#' @return faceted ggplot
CreateHeatMaps <- function(DF, ToMerge = FALSE){#, 
  CatagoryColors <- c("#0571b0","#92c5de", "#979797","WHITE","#f4a582","#ca0020")
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
  return(BarGridSmoothRaw)
}



#' ConfMatrix
#' 
#' creates a confusion matrix from data long format
#'
#' @param DF 
#' @param Cat The column with the values of the methods 
#' @param x The first method to compare
#' @param y The second method to compare
#'
#' @return
ConfMatrix <- function(DF,Cat,x,y){
  RetPlt <- DF%>%
    filter(Method %in% c(x,y))%>%
    select(WWTP,date,Method,Catagory)%>%
    filter(WWTP != "Portage WWTF"  & WWTP != "Cedarburg WWTF")%>%
    pivot_wider(id_cols=c(WWTP,date),names_from = Method, values_from = !!sym(Cat))%>%
    group_by(!!sym(x),!!sym(y))%>%
    summarise(n = n())%>%
    filter(!is.na(!!sym(y)))%>%
    ggplot(aes(x=!!sym(x),y=!!sym(y)))+
    geom_tile(aes(fill = n))+
    scale_fill_gradient(low="blue", high="red")
  return(RetPlt)
}


