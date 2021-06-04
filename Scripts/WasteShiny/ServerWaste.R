                                  
WasteServer <- function(input, output, session) {

  ConfigOption=reactive({
    return(list(
      myseed=1234567890,
      XAxisLabSiz=10,
      YAxisLabSiz=15,
      GenFontSiz=15,
      alphaPoint=.7,
      PointSize=2,
      alphaWeek=.2)
    )
  })
  
  source("../Scripts/WasteShiny/T1WasteGraphs.R",local = T)

  output$plot1<-T1Plot

  source("../Scripts/WasteShiny/T2WasteGraphs.R",local = T)
  
  output$plot2<-T2Plot

  source("../Scripts/WasteShiny/T3WasteGraphs.R",local = T)
  
  output$plot3<-T3Plot
}