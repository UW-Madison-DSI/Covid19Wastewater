#' Returns a df with the multiple ways to analyize how offset the wastewater is from cases data
#'
#' @param length The length of the time window for the results / 2
#' @param startdate First day for both data sets
#' @param enddate Last day for both data sets
#' @param casesdf DF with case data columns needed(data,conf_case)
#' @param wastedf DF with wastewater data columns needed(date,N1,N2)
#'
#' @return DF with the columns: number of days wastewater is offset, geo mean of(n1,n2) / confirmed cases,previous with rolling case average, MSE rolling average,Pearson correlation, Kendall correlation, Spearman correlation, R squared. 
#' @export
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' data("Case_data", package = "DSIWastewater")
#' #Will output a df from -10 to +10 days using all of the data from 2020-08-01 to 2023-01-01
#' OffsetDFMaker(10,as.Date("2020-08-01"), as.Date("2023-01-01"), Case_data, WasteWater_data)
OffsetDFMaker <- function(length, startdate, enddate, casesdf, wastedf){
  #Subset data based on given dates
  wastedf <- subset(wastedf, as.Date(date)> as.Date(startdate) & as.Date(date) < as.Date(enddate))
  casesdf <- subset(casesdf, as.Date(date)> as.Date(startdate) & as.Date(date) < as.Date(enddate))
  #Make columns needed
  wastedf <- wastedf%>% 
    group_by(date)%>% 
    summarise(N1 = mean(N1),
              N2 = mean(N2))%>%
    mutate(geo_mean = exp((log(N1) + log(N2))/2)) %>% 
    drop_na(geo_mean)
  
  casesdf <- casesdf %>%
    group_by(date) %>% 
    summarise(conf_case = mean(conf_case)) %>%
    mutate(rollingaverage = rollmean(conf_case, k=7, fill = NA)) %>%
    drop_na(rollingaverage)
  
  offsetresults <- data.frame(matrix(ncol = 8, nrow = 0))
  #Offset data 1 day at a time and compute values
  for(i in -length:length){
    wasteTemp <- wastedf %>% 
      mutate(tempdate = as.Date(wastedf$date + i))
    
    temp <- merge(casesdf, wasteTemp, by.x = "date" ,by.y = "tempdate") 
    if(count(temp) != 0){
      templm <- lm(log(geo_mean + 1)~log(rollingaverage+1), data=temp) 
      temp <- temp %>% 
        mutate(proportionRolling = temp$geo_mean/temp$rollingaverage,
               proportion = temp$geo_mean/temp$conf_case, 
               mse = mean((temp$geo_mean/temp$rollingaverage - mean(temp$geo_mean/temp$rollingaverage))^2))
      temp <- na.omit(temp)
      lmresult <- tryCatch({
        summary(templm)$r.squared},
        error = function(err) {
          return(0)
        })
      new <- c(i,
               mean(temp$proportion),
               mean(temp$proportionRolling),
               mean(temp$mse), 
               cor(temp$geo_mean,temp$rollingaverage,method = "pearson"), 
               cor(temp$geo_mean,temp$rollingaverage,method = "kendall"),
               cor(temp$geo_mean,temp$rollingaverage,method = "spearman"),
               lmresult)
    } else {
      new <- c(i,NA,NA,NA,NA,NA,NA,NA)
    }
    offsetresults[nrow(offsetresults) + 1, ] <- new  
  }
  offsetresults <- offsetresults %>% 
    rename(wdateoffset = X1,
           wcratio = X2,
           wcratiorolling = X3,
           meanMSErolling = X4,
           corilationPearson = X5,
           corilationKendall = X6,
           corilationSpearman = X7,
           rcor=X8)
  return(offsetresults)
}