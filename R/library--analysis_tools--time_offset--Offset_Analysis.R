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
    rename(wdateoffset=X1,
           wcratio=X2,
           wcratiorolling=X3,
           meanMSErolling=X4,
           corilationPearson=X5,
           corilationKendall=X6,
           corilationSpearman=X7,
           rcor=X8)
  return(offsetresults)
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
#' OffsetDFMaker_Output <- OffsetDFMaker(10,as.Date("2020-08-01"), as.Date("2023-01-01"), Case_data, WasteWater_data)
#' OffsetDF_Plot(OffsetDFMaker_Output,"All Wisconsin data over all time")
OffsetDF_Plot <- function(data,title){
  wc <- ggplot(data, aes(x=wdateoffset, y=wcratio)) +
    geom_point() +
    geom_point(data = data[which.max(data$wcratio),], color = "red") +    
    ylab("Waste-to-case ratio") +
    xlab("Wastewater Offset") +
    ggtitle("Waste-to-case ratio")
  
  wcrolling <- ggplot(data, aes(x=wdateoffset, y=wcratiorolling)) +
    geom_point() + 
    geom_point(data = data[which.min(data$wcratiorolling),], color = "red") +
    ylab("Wastewater to Case ratio") +
    xlab("Wastewater Offset") +
    ggtitle("Rolling waste-to-case ratio") +
    geom_text(data = data[which.min(data$wcratiorolling),], aes(wdateoffset,wcratiorolling, label = paste(wdateoffset, " " ,sep = "\n")))
  
  mse <- ggplot(data, aes(x=wdateoffset, y=meanMSErolling)) +
    geom_point() +
    geom_point(data = data[which.min(data$meanMSErolling),], color = "red") +
    ylab("Rolling Average MSE") +
    xlab("Wastewater Offset") +
    ggtitle("Rolling average MSE") +
    geom_text(data = data[which.min(data$meanMSErolling),], aes(wdateoffset,meanMSErolling, label = paste(wdateoffset, " " ,sep = "\n")))
  
  pearson <- ggplot(data, aes(x=wdateoffset, y=corilationPearson)) +
    geom_point() +
    geom_point(data = data[which.max(data$corilationPearson),], color = "red") +
    ylab("Pearson Correlation") +
    xlab("Wastewater Offset") +
    ggtitle("Pearson") +
    geom_text(data = data[which.max(data$corilationPearson),], aes(wdateoffset,corilationPearson, label = paste(" " ,wdateoffset,sep = "\n")))
  
  kendall <- ggplot(data, aes(x=wdateoffset, y=corilationKendall)) +
    geom_point()+
    geom_point(data = data[which.max(data$corilationKendall),], color = "red") +
    ylab("Kendall correlation") +
    xlab("Wastewater Offset") +
    ggtitle("Kendall") +
    geom_text(data = data[which.max(data$corilationKendall),], aes(wdateoffset,corilationKendall, label = paste(" " ,wdateoffset,sep = "\n")))
  
  spearman <- ggplot(data, aes(x=wdateoffset, y=corilationSpearman)) +
    geom_point() +
    geom_point(data = data[which.max(data$corilationSpearman),], color = "red") +
    ylab("Spearman Correlation") +
    xlab("Wastewater Offset") +
    ggtitle("Spearman") +
    geom_text(data = data[which.max(data$corilationSpearman),], aes(wdateoffset,corilationSpearman, label = paste(" " ,wdateoffset,sep = "\n"))) 
  
  gridOffsetPlots <- grid.arrange(wc,wcrolling,mse,pearson,kendall,spearman,ncol=2,bottom=title)
  return(gridOffsetPlots)
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
  
  p <- ggplot(mdfr, aes(factor(category,levels = c(1:69)), value, fill = variable)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_y_continuous(labels = percent) +
    xlab("Week") +
    ylab("Covariant Percent")
  
  return(ggplotly(p))
}

###### Correlation Offset Heatmap
#' Outputs a heatmap where the color is the r squared of wastewater data and center day + x many future days and y many past days
#' Helps inform Offset Analysis
#' @param cordata DF with geo_mean and conf_case columns 
#'
#' @return ggplot plot object (heatmap)
#' @export
#' 
#' @examples
#'  data(example_data, package = "DSIWastewater")
#'  heatmapcorfunc(Example_data)
heatmapcorfunc <- function(cordata,length=14){
  
  rsquareddf <- data.frame(matrix(ncol = 1, nrow = 0))
  for(j in 0:length){
    for(i in 0:length){
      wctemplm <- cordata %>% 
        mutate(tempsum = roll_sum(conf_case,i,align = "left",fill = NA) + 
                 roll_sum(conf_case,j,align = "right",fill = NA) + 
                 roll_sum(conf_case,1,align = "center",fill = NA)) 
      
      templm <- lm(log(tempsum+1)~log(geo_mean+1), data=wctemplm)
      new <- c(i,j,summary(templm)$r.squared)
      rsquareddf = rbind(rsquareddf,new)
    }
  }
  
  names(rsquareddf)[1]="left" #(future)
  names(rsquareddf)[2]="right" #(past)
  names(rsquareddf)[3]="rsquared"
  heatmapcor <- rsquareddf %>% ggplot(aes(x = left, y = right, fill = rsquared))+
    geom_tile() + 
    scale_fill_gradientn(colours=rainbow(7)) + 
    xlab("Future Days") + 
    ylab("Past Days") + 
    ggtitle("Correlation of past and future days of N1 and N2 to current day cases")
  
  return(heatmapcor)
}


###### Offset Heatmap
#' Outputs a heatmap of the offset for variant / time windows and population size / region
#' Must have pop_data, covarstarts, covarends, covarnames loaded
#'
#' @param method Which analysis definds the offset (r squared, pearson, r squared offset, pearson offset, kendall offset, spearman offset)
#' @param timePeriods Size of time windows in months (if 0 uses variants)
#' @param wasterv DF of waste data must include: date, N1, N2
#' @param caserv DF of case data must include: date, conf_case
#' @param list y axis bins (population size or regions)
#' @param lod if true removes all values below LOD (default false)
#' @param week if true applies 7-day smoothing to case data 
#' 
#' @return ggplot plot object 
#' @export
#' @examples
#' data(WasteWater_data, package = "DSIWastewater")
#' data("Case_data", package = "DSIWastewater")
#' data(pop_data, package = "DSIWastewater")
#' OffsetHeatmap("kendall_offset",0,waste,cases,"pop",TRUE,TRUE)
#' OffsetHeatmap("kendall_offset",0,waste,cases,"pop",TRUE,TRUE)
#' 
OffsetHeatmap <- function(method, timePeriods,wasterv,caserv,list,lod=FALSE,week){
  
  outputrvt <- data.frame(matrix(ncol = 1, nrow = 0))
  
  tempregion <- select(wasterv,regions,site) %>% unique()
  if(list == "region"){
    regionslist <- c("all", unique(wasterv$regions))
  } else {#pop
    populationtemp <- pop_data %>% mutate(popgroup = case_when(pop >= 100000 ~ "Large",
                                                               pop >= 10000  ~ "Medium",
                                                               .default = "Small")) %>% subset(select = c(site,popgroup))
    
    wasterv <- merge(wasterv,populationtemp, by.x = "site") %>% mutate(regions = popgroup)
    
    regionslist <- c("all", unique(wasterv$regions))#
  }
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
      wastervtemp <- subset(wasterv, as.Date(date)> as.Date(start[i]) & as.Date(date) < as.Date(end[i]))
      caservtemp <- subset(caserv, as.Date(date)> as.Date(start[i]) & as.Date(date) < as.Date(end[i]))
      
      if(lod){
        wastervtemp <- wastervtemp %>% filter(N1 > n1_lod)
      }
      
      if(j == "all"){
        wastervtemp <- wastervtemp %>% group_by(date)
      } else {
        wastervtemp <- wastervtemp %>% group_by(date,regions) %>% filter(regions == j)
      }
      wastervtemp <- wastervtemp %>% 
        summarise(N1 = mean(N1),
                  N2 = mean(N2),
                  geo_mean = exp((log(N1) + log(N2))/2) ) %>% 
        drop_na(geo_mean)
      if(week == TRUE){
        wastervtemp <- wastervtemp %>% mutate(geo_mean = roll_mean(geo_mean,7,align = "center", fill = NA))
      }
      if(j == "all"){
        caservtemp <- merge(tempregion,caservtemp,by = "site") %>%
          group_by(date) %>% 
          summarise(conf_case = mean(conf_case)) 
      } else {
        caservtemp <- merge(tempregion,caservtemp,by = "site") %>% filter(regions == j) %>%
          group_by(date,regions) %>% 
          summarise(conf_case = mean(conf_case)) 
      }
      if(week == TRUE){
        caservtemp <- caservtemp %>% ungroup() %>%
          mutate(conf_case = roll_mean(conf_case,7,align = "center", fill = NA))
      }
      
      caservtemp <- caservtemp %>% ungroup() %>%
        mutate(roll_test = roll_sum(conf_case,3,align = "center",fill = NA),
               rollingaverage = (roll_sum(conf_case,3,align = "center",fill = NA)/roll_test)) %>%
        na.omit()
      
      regionvstimetemp <- merge(caservtemp,wastervtemp, by = "date")
      
      if(method == "rsquared_cor"){
        na.omit(regionvstimetemp)
        templm <- lm(log(geo_mean + 1)~log(rollingaverage+1), data=regionvstimetemp) 
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
      outputrvt = rbind(outputrvt,new)
    }
  }
  
  names(outputrvt)[1]="time" 
  names(outputrvt)[2]="region" 
  names(outputrvt)[3]="output"
  names(outputrvt)[4]="rcor"
  outputrvt$output <- as.numeric(outputrvt$output)
  outputrvt$time <- as.numeric(outputrvt$time)
  newend <- as.numeric(end) - as.numeric(start[1])
  newstart <- as.numeric(start) - as.numeric(start[1])
  newdatesdf <- data.frame(time = as.numeric(1:length(newend)), end = newend, start = newstart)
  outputrvt <- left_join(outputrvt,newdatesdf)
  outputrvt <- outputrvt %>% mutate(regionsizemin = case_when(region == "Southeastern" ~ 0,
                                                              region == "Northern" ~ 1,
                                                              region == "Northeastern" ~ 2,
                                                              region == "Western" ~ 3,
                                                              region == "Southern" ~ 4,
                                                              region == "all" ~ 6,
                                                              region == "Small" ~ 0,
                                                              region == "Medium" ~ 1,
                                                              region == "Large" ~ 2),
                                    regionsizemax = regionsizemin+1)
  if(list == "region"){
    templabels=c("0" = "Southeastern", "1" = "Northern","2" = "Northeastern","3" = "Western","4" = "Southern","5" = " ",  "6" = "All Sites","7" = " ")
  } else {
    templabels=c("0" = "Small\n(<10,000)", "1" = "Medium\n(10,000 - 100,000)","2" = "Large\n(>100,000)","3" = " ","4" = " ","5" = " ",  "6" = "All Sites","7" = " ")
  }
  
  heatmaprvt <- ggplot(data = outputrvt, aes(fill = output,)) +
    geom_rect(aes(ymin = as.factor(regionsizemin), ymax = as.factor(regionsizemax), xmin = as.Date(start + firstday), xmax = as.Date(end + firstday))) +
    geom_text(aes(x = (as.Date(start + firstday) + (as.Date(end + firstday) - as.Date(start + firstday)) / 2),
                  y = case_when(regionsizemax < 5 ~ (regionsizemin + (regionsizemax - regionsizemin) / 2)+1,
                                .default = (regionsizemin + (regionsizemax - regionsizemin) / 2)-1), 
                  label = case_when(is.na(rcor) ~ paste(NA),
                                    .default = paste(round(as.numeric(rcor), digits=2)))),
              size = 3) +
    scale_fill_gradient2(low = "#D16BA5", mid = "#86A8E7", high = "#5FFBF1", na.value = "white" ) + 
    xlab(case_when(timePeriods == 0 ~ paste("Variants (over 50% of population)"),
                   .default = paste(timePeriods, " Month Periods (Since August 1st 2020)"))) + 
    scale_y_discrete(labels=templabels) +
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

