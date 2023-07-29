#' Returns a dataframe with the multiple ways to analyze how offset the Wastewater is from cases data
#'
#' @param length The length of the time window for the results / 2
#' @param startdate First day for both data sets
#' @param enddate Last day for both data sets
#' @param casesdf DF with case data columns needed(data,conf_case)
#' @param wastedf DF with Wastewater data columns needed(date,N1,N2)
#' @param N1_column name of N1 column
#' @param N2_column name of N2 column
#' @param site_column name of site column
#' @param date_column name of date column
#' @param case_column name of case column
#'
#' @return DF with the columns: number of days wastewater is offset, geo mean of(n1,n2) / confirmed cases,previous with rolling case average, MSE rolling average,Pearson correlation, Kendall correlation, Spearman correlation, R squared. 
#' @export
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' data("Case_data", package = "DSIWastewater")
#' #Will output a df from -10 to +10 days using all of the data from 2020-08-01 to 2023-01-01
#' OffsetDFMaker(10,as.Date("2020-08-01"), as.Date("2023-01-01"), Case_data, WasteWater_data) 
OffsetDFMaker <- function(length, startdate, enddate, casesdf, wastedf,
                          N1_column = N1,
                          N2_column = N2, 
                          site_column = site,
                          date_column = date, 
                          case_column = conf_case){
  
  N1 <- N2 <- site <- date <- conf_case <- NA #default column for function. Not evaluated as NA in dplyr context
  
  #Subset data based on given dates
  wastedf <- subset(wastedf, as.Date(date)> as.Date(startdate) & as.Date(date) < as.Date(enddate))
  casesdf <- subset(casesdf, as.Date(date)> as.Date(startdate) & as.Date(date) < as.Date(enddate))
  
  #Make columns needed
  wastedf <- wastedf%>% 
    group_by({{date_column}})%>% 
    summarise(N1 = mean({{N1_column}}),
              N2 = mean({{N2_column}}))%>%
    mutate(geo_mean = exp((log(.data$N1) + log(.data$N2))/2)) %>% 
    drop_na(.data$geo_mean)
  
  casesdf <- casesdf %>%
    group_by(date) %>% 
    summarise(conf_case = mean({{case_column}})) %>%
    mutate(rollingaverage = rollmean(.data$conf_case, k=7, fill = NA)) %>%
    drop_na(.data$rollingaverage)
  
  offset_results <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(offset_results) <- c('wdateoffset', 'wcratio', 'wcratiorolling', 'meanMSErolling',
                                'corilationPearson', 'corilationKendall', 'corilationSpearman', 
                                'rcor')
  
  #Offset data 1 day at a time and compute values
  for(i in -length:length){
    waste_shift_df <- wastedf %>% 
      mutate(shift_date = as.Date(wastedf$date + i))
    
    
    merged_df <- merge(casesdf, waste_shift_df, by.x = "date" ,by.y = "shift_date") 
    if(count(merged_df) != 0){
      merged_dflm <- lm(log(geo_mean + 1) ~ log(rollingaverage+1), data=merged_df) 
      merged_df <- merged_df %>% 
        mutate(proportionRolling = merged_df$geo_mean/merged_df$rollingaverage,
               proportion = merged_df$geo_mean/merged_df$conf_case, 
               mse = mean((merged_df$geo_mean/merged_df$rollingaverage - mean(merged_df$geo_mean/merged_df$rollingaverage))^2))
      merged_df <- na.omit(merged_df)
      lmresult <- tryCatch({
        summary(merged_dflm)$r.squared},
        error = function(err) {
          return(0)
        })
      new <- c(i,
               mean(merged_df$proportion),
               mean(merged_df$proportionRolling),
               mean(merged_df$mse), 
               cor(merged_df$geo_mean,merged_df$rollingaverage,method = "pearson"), 
               cor(merged_df$geo_mean,merged_df$rollingaverage,method = "kendall"),
               cor(merged_df$geo_mean,merged_df$rollingaverage,method = "spearman"),
               lmresult)
    } else {
      new <- c(i,NA,NA,NA,NA,NA,NA,NA)
    }
    offset_results[nrow(offset_results) + 1, ] <- new  
  }
  return(offset_results)
}


###### Correlation Offset Heatmap
#' Outputs a heatmap where the color is the r squared of wastewater data and center day + x many future days and y many past days
#' Helps inform Offset Analysis
#'
#' @param cordata DF with geo_mean and conf_case columns 
#' @param length the length of the time window for the results / 2
#' @param case_column name of case column
#'
#' @return ggplot plot object (heatmap)
#' @export
#' 
#' @examples
#'  data(example_data, package = "DSIWastewater")
#'  heatmapcorfunc(Example_data)
heatmapcorfunc <- function(cordata,length=14, case_column = conf_case){
  conf_case <- NA
  
  rsquareddf <- data.frame(matrix(ncol = 1, nrow = 0))
  for(j in 0:length){
    for(i in 0:length){
      wctemplm <- cordata %>% 
        mutate(tempsum = roll_sum({{case_column}},i,align = "left",fill = NA) + 
                 roll_sum({{case_column}},j,align = "right",fill = NA) + 
                 roll_sum({{case_column}},1,align = "center",fill = NA)) 
      
      templm <- lm(log(tempsum+1) ~ log(geo_mean+1), data = wctemplm)
      new <- c(i,j,summary(templm)$r.squared)
      rsquareddf = rbind(rsquareddf,new)
    }
  }
  
  names(rsquareddf)[1]="left" #(future)
  names(rsquareddf)[2]="right" #(past)
  names(rsquareddf)[3]="rsquared"
  heatmapcor <- rsquareddf %>% ggplot(aes(x = .data$left, y = .data$right, fill = .data$rsquared))+
    geom_tile() + 
    scale_fill_gradientn(colours=rainbow(7)) + 
    xlab("Future Days") + 
    ylab("Past Days") + 
    ggtitle("Correlation of past and future days of N1 and N2 to current day cases")
  
  return(heatmapcor)
}