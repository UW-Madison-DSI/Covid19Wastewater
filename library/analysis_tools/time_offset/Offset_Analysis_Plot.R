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
#'  data(Example_data, package = "DSIWastewater")
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
  heatmapcor <- rsquareddf %>% 
    ggplot(aes(x = .data$left, y = .data$right, fill = .data$rsquared))+
    geom_tile() + 
    scale_fill_gradientn(colours=rainbow(7)) + 
    xlab("Future Days") + 
    ylab("Past Days") + 
    ggtitle("Correlation of past and future days of N1 and N2 to current day cases")
  
  return(heatmapcor)
}

#' Given output from OffsetDFMaker returns a 2x3 grid of all the plots with highlighted values
#'
#' @param data Output from OffsetDFMaker
#' @param title Title you want (appears at bottom)
#'
#' @return ggplot plot object
#' @export
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' data("Case_data", package = "DSIWastewater")
#' OffsetDFMaker_Output <- OffsetDFMaker(10, as.Date("2020-08-01"),
#'                        as.Date("2023-01-01"),
#'                         Case_data, WasteWater_data)
#' OffsetDF_Plot(OffsetDFMaker_Output,"All Wisconsin data over all time")
OffsetDF_Plot <- function(data, title){
  wc <- ggplot(data, aes(x= .data$wdateoffset, y = .data$wcratio)) +
    geom_point() +
    geom_point(data = data[which.max(data$wcratio),], color = "red") +    
    ylab("Waste-to-case ratio") +
    xlab("Wastewater Offset") +
    ggtitle("Waste-to-case ratio")
  
  wcrolling <- ggplot(data, aes(x = .data$wdateoffset, y = .data$wcratiorolling)) +
    geom_point() + 
    geom_point(data = data[which.min(data$wcratiorolling),], color = "red") +
    ylab("Wastewater to Case ratio") +
    xlab("Wastewater Offset") +
    ggtitle("Rolling waste-to-case ratio") +
    geom_text(data = data[which.min(data$wcratiorolling),], 
              aes(.data$wdateoffset, .data$wcratiorolling, 
                  label = paste(.data$wdateoffset, " " ,sep = "\n")))
  
  mse <- ggplot(data, aes(x = .data$wdateoffset, y = .data$meanMSErolling)) +
    geom_point() +
    geom_point(data = data[which.min(data$meanMSErolling),], color = "red") +
    ylab("Rolling Average MSE") +
    xlab("Wastewater Offset") +
    ggtitle("Rolling average MSE") +
    geom_text(data = data[which.min(data$meanMSErolling),], 
              aes(.data$wdateoffset, .data$meanMSErolling,
                  label = paste(.data$wdateoffset, " " ,sep = "\n")))
  
  pearson <- ggplot(data, aes(x = .data$wdateoffset, y = .data$corilationPearson)) +
    geom_point() +
    geom_point(data = data[which.max(data$corilationPearson),], color = "red") +
    ylab("Pearson Correlation") +
    xlab("Wastewater Offset") +
    ggtitle("Pearson") +
    geom_text(data = data[which.max(data$corilationPearson),], 
              aes(.data$wdateoffset, .data$corilationPearson, 
                  label = paste(" " , .data$wdateoffset,sep = "\n")))
  
  kendall <- ggplot(data, aes(x = .data$wdateoffset, y = .data$corilationKendall)) +
    geom_point()+
    geom_point(data = data[which.max(data$corilationKendall),], color = "red") +
    ylab("Kendall correlation") +
    xlab("Wastewater Offset") +
    ggtitle("Kendall") +
    geom_text(data = data[which.max(data$corilationKendall),], 
              aes(.data$wdateoffset, .data$corilationKendall,
                  label = paste(" " , .data$wdateoffset,sep = "\n")))
  
  spearman <- ggplot(data, 
                     aes(x = .data$wdateoffset, y = .data$corilationSpearman)) +
    geom_point() +
    geom_point(data = data[which.max(data$corilationSpearman),], color = "red") +
    ylab("Spearman Correlation") +
    xlab("Wastewater Offset") +
    ggtitle("Spearman") +
    geom_text(data = data[which.max(data$corilationSpearman),], 
              aes(.data$wdateoffset, .data$corilationSpearman, 
                  label = paste(" " , .data$wdateoffset, sep = "\n"))) 
  
  return(grid.arrange(wc, wcrolling, mse,pearson, kendall, spearman, ncol=2, bottom=title))
}
###### Variant plots?
#' Shows each variant in proportion to the others in 2 week time periods
#'
#' @param covar Covariant data frame
#'
#' @return ggplotly object
#' @export
#' @examples
#' data(Covariants_data, package = "DSIWastewater")
#' VariantPlot(Covariants_data)
VariantPlot <- function(covar){
  covar$category <- row.names(covar)
  onlycovar <- covar[-c(1,2)]
  mdfr <- melt(onlycovar, id.vars = "category")
  
  p <- ggplot(mdfr, aes(factor(.data$category, levels = c(1:69)), .data$value, fill = .data$variable)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = label_percent()) +
    xlab("Week") +
    ylab("Covariant Percent")
  
  return(ggplotly(p))
}


###### Offset Heatmap
#' Outputs a heatmap of the offset for variant / time windows and population size / region
#'
#' @param method Which analysis definds the offset (r squared, pearson, r squared offset, pearson offset, kendall offset, spearman offset)
#' @param timePeriods Size of time windows in months (if 0 uses variants)
#' @param waste_df DF of waste data must include: date, N1, N2
#' @param case_df DF of case data must include: date, conf_case
#' @param list y axis bins (population size or regions)
#' @param lod if true removes all values below LOD (default false)
#' @param week if true applies 7-day smoothing to case data 
#' @param pop_df dataframe where region and population info is stored
#' @param covarstarts start of each split period
#' @param covarends end of each split period
#' @param covarnames name of each split group
#' @param N1_column name of N1 column
#' @param N2_column name of N2 column
#' @param site_column name of site column
#' @param date_column name of date column
#' @param case_column name of case column
#' @param pop_column name of pop column
#' 
#' @return ggplot plot object 
#' @export
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' data("Case_data", package = "DSIWastewater")
#' data(pop_data, package = "DSIWastewater")
#' covarstarts <- c(as.Date("2020-08-17"),
#'                  as.Date("2021-03-29"))
#' covarends <- c(as.Date("2021-01-18"),
#'                as.Date("2021-05-24"))
#' covarnames <- c("Robin1",
#'                 "Alpha.V1")
#' OffsetHeatmap("kendall_offset", 0, WasteWater_data, Case_data, pop_data,
#'                covarstarts, covarends, covarnames, "pop", TRUE, TRUE)
OffsetHeatmap <- function(method, timePeriods, waste_df, case_df, pop_df, 
                          covarstarts, covarends, covarnames,
                          list, week, lod = FALSE, 
                          N1_column = N1,
                          N2_column = N2, 
                          site_column = site,
                          date_column = date, 
                          case_column = conf_case,
                          pop_column = pop){
  N1 <- N2 <- site <- date <- pop <- conf_case <- NA #default column for function. Not evaluated as NA in dplyr context
  
  merged_waste_df <- waste_df%>%
    left_join(pop_df)
  
  outputrvt <- data.frame(matrix(ncol = 1, nrow = 0))
  

  if(list == "region"){
    regionslist <- c("all", unique(waste_df$regions))
  } else {#pop
    merged_waste_df <- merged_waste_df %>% 
      mutate(regions = case_when({{pop_column}} >= 100000 ~ "Large",
                                  {{pop_column}} >= 10000  ~ "Medium",
                                  .default = "Small"))
    
    regionslist <- c("all", unique(merged_waste_df$regions))#
  }
  
  tempregion <- merged_waste_df%>%
    select(.data$regions, {{site_column}}) %>% 
    unique()
  
  firstday <- as.numeric(as.Date("2020-08-01"))
  endday <- as.numeric(as.Date("2022-12-01"))
  
  if(timePeriods == 0){
    start <- covarstarts
    end <- covarends
  } else {
    start <- c()
    end <- c()
    curtempdate <- firstday
    while(curtempdate + 20 < endday){
      start <- append(start, curtempdate)
      curtempdate <- curtempdate + as.numeric(timePeriods) * 30
      end <- append(end, curtempdate)
      curtempdate <- curtempdate + 1
    }
    start <- as.Date(start)
    end <- as.Date(end)
    
  }
  
  for(j in regionslist){
    for(i in 1:length(start)){
      wastervtemp <- subset(merged_waste_df, as.Date(date)> as.Date(start[i]) & as.Date(date) < as.Date(end[i]))
      caservtemp <- subset(case_df, as.Date(date)> as.Date(start[i]) & as.Date(date) < as.Date(end[i]))
      
      if(lod){
        wastervtemp <- wastervtemp %>% 
          filter({{N1_column}} > .data$n1_lod)
      }
      
      if(j != "all"){
        wastervtemp <- wastervtemp%>% 
          filter(.data$regions == j) 
      }
      wastervtemp <- wastervtemp %>% 
        group_by({{date_column}})%>% 
        summarise(N1 = mean({{N1_column}}),
                  N2 = mean({{N2_column}}),
                  geo_mean = exp((log(.data$N1) + log(.data$N2))/2) ) %>% 
        drop_na(.data$geo_mean)
      
      caservtemp <- merge(tempregion, caservtemp, by = "site")
      
      if(j != "all"){
        caservtemp <- caservtemp %>% 
          filter(.data$regions == j)
      } 

      caservtemp <- caservtemp %>%
        group_by({{date_column}}) %>% 
        summarise(conf_case = mean({{case_column}})) 
      
      if(week == TRUE){#If week is true we want the weekly average instead
        caservtemp <- caservtemp %>% 
          ungroup() %>%
          mutate(conf_case = roll_mean({{case_column}},7, align = "center", fill = NA))
        wastervtemp <- wastervtemp %>% 
          mutate(geo_mean = roll_mean(.data$geo_mean, 7, align = "center", fill = NA))
      }
      
     caservtemp <- caservtemp %>% 
        ungroup() %>%
        mutate(roll_test = roll_sum(conf_case, 3, align = "center",fill = NA),
               rollingaverage = (roll_sum(conf_case,3, align = "center",fill = NA) / .data$roll_test)) %>%
        na.omit()
      
      regionvstimetemp <- merge(caservtemp, wastervtemp, by = "date")
      
      if(method == "rsquared_cor"){
        na.omit(regionvstimetemp)
        templm <- lm(log(geo_mean + 1) ~ log(rollingaverage+1), data=regionvstimetemp) 
        new <- c(i,j,summary(templm)$r.squared)
      } else if(method == "pearson_cor"){
        tempcor <- cor(regionvstimetemp$rollingaverage,regionvstimetemp$geo_mean,method = "pearson")
        new <- c(i,j,tempcor)
      } else if(method == "pearson_offset"){
        offsetOutput <- OffsetDFMaker(10, start[i], end[i], caservtemp, wastervtemp)
        offsetOutput <- offsetOutput[order(-offsetOutput$corilationPearson),]
        new <- c(i,j,offsetOutput[1, "wdateoffset"])
      } else if(method == "offset_rcor"){
        offsetOutput <- OffsetDFMaker(10, start[i], end[i], caservtemp, wastervtemp)
        offsetOutput <- offsetOutput[order(-offsetOutput$rcor),]
        new <- c(i,j,offsetOutput[1, "wdateoffset"])
      } else if(method == "spearman_offset") {
        offsetOutput <- OffsetDFMaker(10, start[i], end[i], caservtemp, wastervtemp)
        offsetOutput <- offsetOutput[order(-offsetOutput$corilationSpearman),]  
        new <- c(i,j,offsetOutput[1, "wdateoffset"])
      } else if(method == "kendall_offset"){
        offsetOutput <- OffsetDFMaker(12, start[i], end[i], caservtemp, wastervtemp)
        offsetOutput <- offsetOutput[order(-offsetOutput$corilationKendall),]
        new <- c(i,j,offsetOutput[1, "wdateoffset"],offsetOutput[1, "corilationKendall"])
      }
      outputrvt = rbind(outputrvt, new)
    }
  }
  
  names(outputrvt)[1:4] = c("time", "region", "output", "rcor")

  outputrvt$output <- as.numeric(outputrvt$output)
  outputrvt$time <- as.numeric(outputrvt$time)
  newend <- as.numeric(end) - as.numeric(start[1])
  newstart <- as.numeric(start) - as.numeric(start[1])
  newdatesdf <- data.frame(time = as.numeric(1:length(newend)), end = newend, start = newstart)
  outputrvt <- left_join(outputrvt, newdatesdf)
  outputrvt <- outputrvt %>% 
    mutate(regionsizemin = case_when(.data$region == "Southeastern" ~ 0,
                                     .data$region == "Northern" ~ 1,
                                     .data$region == "Northeastern" ~ 2,
                                     .data$region == "Western" ~ 3,
                                     .data$region == "Southern" ~ 4,
                                     .data$region == "all" ~ 6,
                                     .data$region == "Small" ~ 0,
                                     .data$region == "Medium" ~ 1,
                                     .data$region == "Large" ~ 2),
           regionsizemax = .data$regionsizemin + 1)
  if(list == "region"){
    templabels=c("0" = "Southeastern", "1" = "Northern","2" = "Northeastern","3" = "Western","4" = "Southern","5" = " ",  "6" = "All Sites","7" = " ")
  } else {
    templabels=c("0" = "Small\n(<10,000)", "1" = "Medium\n(10,000 - 100,000)","2" = "Large\n(>100,000)","3" = " ","4" = " ","5" = " ",  "6" = "All Sites","7" = " ")
  }
  
  heatmaprvt <- ggplot(data = outputrvt, aes(fill = .data$output,)) +
    geom_rect(aes(ymin = as.factor(.data$regionsizemin),
                  ymax = as.factor(.data$regionsizemax),
                  xmin = as.Date(start + firstday),
                  xmax = as.Date(end + firstday))) +
    geom_text(aes(x = (as.Date(start + firstday) + (as.Date(end + firstday) - as.Date(start + firstday)) / 2),
                  y = case_when(.data$regionsizemax < 5 ~ (.data$regionsizemin + (.data$regionsizemax - .data$regionsizemin) / 2) + 1,
                                .default =                (.data$regionsizemin + (.data$regionsizemax - .data$regionsizemin) / 2) - 1), 
                  label = case_when(is.na(.data$rcor) ~ paste(NA),
                                    .default = paste(round(as.numeric(.data$rcor), digits=2)))),
              size = 3) +
    scale_fill_gradient2(low = "#D16BA5", mid = "#86A8E7", high = "#5FFBF1", na.value = "white" ) + 
    xlab(case_when(timePeriods == 0 ~ paste("Variants (over 50% of population)"),
                   .default = paste(timePeriods, " Month Periods (Since August 1st 2020)"))) + 
    scale_y_discrete(labels = templabels) +
    ylab(case_when(list == "region" ~ "Region",
                   .default = "Population")) +
    ggtitle(case_when(lod == TRUE ~ paste(method, "values above LOD"),
                      .default = paste(method, "using all N1/N2") ) )  +
    labs(fill="") #Days wastewater is offset from cases 
  geom_text(aes(x = (as.Date(start + firstday) + (as.Date(end + firstday) - as.Date(start + firstday)) / 2),
                y = .75, 
                label = paste(covarnames[time]), angle = 30), size = 2)
  
  return(heatmaprvt)
}
