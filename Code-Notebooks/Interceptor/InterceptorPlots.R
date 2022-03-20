
N1ShapeUnit = 4
N2ShapeUnit = 5

#ReUsed Formating Functions
DefaultPlotSettings <- function(){#Should be done to every plot
  return(theme_light())
}
OutlierColorScheme <- function(){#Color scheme for a outlier vs inlier plot
  return(scale_color_manual(values=c("#F8766D","#000000")))
}
SiteColorScheme <- function(ExtremeP18 = FALSE){#Color scheme for a intercepter plot
  n <- 5
  RotSpacing <- seq(15, 375, length = n+1)#Getting equal spaced colors
  HueSpace <- hcl(h = RotSpacing, l = 65, c = 100)[1:n]#converting degrees to color
  HueSpace <- c(tail(HueSpace, -3), head(HueSpace, 3))#Rotating so P18 has best color
  if(ExtremeP18){
    HueSpace[2] <- "#000000" #Changing P18 to black k
  }
  return(scale_color_manual(values=HueSpace, aesthetics = c("fill","colour")))
}

YLabN1_N2 <- function(){#Lab when ploting both N1 and N2 data
  return(ylab("GC/L"))
}


#Monday Presentation Plots
OutLier_pointPlot <- function(DF, MainComp,SubComp, OutlierMain=NA, 
                              OutlierSub=NA, SizeUsed = .1){
  if(is.na(OutlierMain)){
    OutlierMain <- paste0("Outlier",MainComp)
  }
  if(is.na(OutlierSub)){
    OutlierSub <- paste0("Outlier",SubComp)
  }
  

  ReturnPlots <- DF%>%
    ggplot(aes(x=Date))+#Data depends on time 
    geom_point(aes(y=!!sym(MainComp), color="inlier", info = !!sym(MainComp), shape= MainComp), 
               size = SizeUsed)+
    geom_point(aes(y=!!sym(SubComp), color="inlier",  info = !!sym(SubComp), shape= SubComp), 
               size = SizeUsed )+
    geom_point(aes(y=!!sym(OutlierMain), color="Outlier", info = !!sym(OutlierMain), shape=MainComp),
                size = 2*SizeUsed )+
    geom_point(aes(y=!!sym(OutlierSub), color="Outlier", info = !!sym(OutlierSub), shape= SubComp),
               size = 2*SizeUsed)+
    scale_shape_manual(values = c(N1ShapeUnit,N2ShapeUnit))+
    DefaultPlotSettings()+
    OutlierColorScheme()+
    YLabN1_N2()
  return(ReturnPlots)
}


MassBalence_BarPlot <- function(DF, MainComp, Break, SubComp=NA, SizeUsed = .5){
  if(is.na(SubComp)){
    SubComp = paste0("Selected",MainComp)
  }
  ReturnPlots <- DF%>%
    ggplot()+
    geom_col(aes(x = Date,y = !!sym(MainComp), fill = !!sym(Break)), 
             position="stack",width = 3) + 
    geom_point(aes(x = Date,y = !!sym(SubComp)), size=SizeUsed, color = "Black")+
    SiteColorScheme() +
    DefaultPlotSettings() +
    YLabN1_N2()
  return(ReturnPlots)
}


pointPlotSite <- function(DF, MainComp,SubComp,Break, SizeUsed = .5){
  ReturnPlots <- DF%>%
    ggplot(aes(x=Date))+#Data depends on time 
    geom_point(aes(y=!!sym(MainComp), color= !!sym(Break), info = !!sym(MainComp), shape= MainComp), 
               size = SizeUsed)+
    geom_point(aes(y=!!sym(SubComp), color= !!sym(Break),  info = !!sym(SubComp), shape= SubComp), 
               size = SizeUsed)+
    scale_shape_manual(values = c(N1ShapeUnit,N2ShapeUnit))+
    DefaultPlotSettings() + 
    SiteColorScheme(ExtremeP18 = TRUE) +
    YLabN1_N2()
  return(ReturnPlots)
}

LinePlotSite <- function(DF, MainComp,SubComp,Break){
  ReturnPlots <- DF%>%
    ggplot(aes(x=Date))+#Data depends on time 
    geom_line(aes(y=!!sym(MainComp), color = !!sym(Break), linetype  = MainComp))+
    geom_line(aes(y=!!sym(SubComp), color = !!sym(Break), linetype  = SubComp))+
    scale_shape_manual(values = c(N1ShapeUnit,N2ShapeUnit))+
    DefaultPlotSettings() + 
    SiteColorScheme(ExtremeP18 = TRUE) +
    YLabN1_N2()
  return(ReturnPlots)
}
