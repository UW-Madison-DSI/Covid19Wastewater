#' create rolling quantile column based on date
#'
#'Takes 1 time series dataframe and adds a some columns specifying the 
#'rolling quantile and rolling mean of the selected column.
#'
#' @param DF dataframe containing columns:
#' date: date variable communicating the day the measurement is from
#' window: singular type column saying what window the rolling function
#'  should use
#' quant: singular value column saying what quantile to return
#' *column: DF needs to contain a column with the same name as the string
#' in the variable column
#' 
#' @param column  what column to use for the rolling quantile and mean functions
#'
#' @return DF with added columns
#' ntile: a rolling quantile of the data
#' pastKavg.wwlog10: a mean of the last K days
#' @keywords internal
#' @examples 
#' data("example_data", package = "DSIWastewater")
#' example_data$window = 7
#' example_data$quant = .8
#' DSIWastewater:::windowingQuantFunc(example_data, "geoMean")
windowingQuantFunc <- function(DF, column){
  #get the start of the time series
  mindate <- min(DF$date, na.rm = TRUE)
  #get the end of the time series
  maxdate <- max(DF$date, na.rm = TRUE)
  #get what window to use. min should equal max
  FuncWindow <- min(DF$window, na.rm = TRUE)
  #get what quant to use. min should equal max
  FuncQuant <- min(DF$quant, na.rm = TRUE)
  
  #get a dataframe that contains all dates in range of DF
  dateTOMERGEVec <- data.frame(date = seq(mindate, maxdate, 1))
  
  #how many days the rolling mean should include
  K = 3
  
  #Merge DF to add rows for each date 
  RetDF <- DF%>%
    left_join(dateTOMERGEVec, . , by = c("date"))%>%
    #sort by date so the rolling functions work correctly
    arrange(date)%>%
    #add ntile column that is the window day Quant quantile of the column column
    mutate(ntile = rollapply(!!sym(column),
                     width = FuncWindow,
                     FUN = quantile, 
                     probs  = FuncQuant,
                     #align right so its not predictive
                     align = "right", 
                     #remove outliers and fill edges with NA
                     #so it does not return NA
                     na.rm=TRUE, fill=NA,
                     #returns as a vector instead of a list
                     names = FALSE))%>%
    #removes all extra rows created that were used in rolling process
    filter(!is.na(population_served))
    
  return(RetDF)
}

#' Add many combo of rolling quantile columns to dataframe
#' have info for each quant window combo 
#'
#' @param DF Dataframe containing columns:
#' site: what site the data is from
#' date: date variable communicating the day the measurement is from
#' *column: DF needs to contain a column with the same name as the string
#' in the variable column
#' 
#' @param column string name of column in DF
#' @param quants vector containing the different quantiles
#' @param windows vector containing the different windows
#'
#' @return DF with added columns
#' window:what window group the row is in
#' quant:what quantile group the row is in
#' ntile: a rolling quantile of the data
#' pastKavg.wwlog10: a mean of the last K days
#' @export
#' @examples
#' data("example_data", package = "DSIWastewater")
#' example_data$site = "madison"
#' makeQuantileColumns(example_data, .5, 6, column = "geoMean")
#' makeQuantileColumns(example_data, c(.5, .75), c(2,5), column = "geoMean")
makeQuantileColumns <- function(DF, quants, windows,
                                column = "sars_cov2_adj_load_log10"){
  #create a DF with every combo of windows, quants, site
  #Used to merge with DF to get a DF length(quants)*length(windows) times longer
  Method_DF <- expand.grid(windows, 
                           quants, 
                           unique(DF$site))
  
  #rename columns of merging DF to help the full joing function
  colnames(Method_DF) <- c("window", "quant", "site")
  
  
  Quantiles_DF <- DF%>%
    #merge with Method_DF to get the right number of rows for incoming split
    full_join(Method_DF, by = c("site"))%>%
    #split the data by site,Window,quant so each DF has one time series
    split(~site + window + quant)%>%
    #Feed each time series into the windowingQuantFunc to get the actualy quant
    lapply(windowingQuantFunc, column = column)%>%
    #append the DF back together to return
    bind_rows()
  return(Quantiles_DF)
}
