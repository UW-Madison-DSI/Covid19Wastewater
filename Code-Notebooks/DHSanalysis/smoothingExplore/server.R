
library(shiny)
library(DSIWastewater)
library(dplyr)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  getData <- reactive({
    data(wastewater_data, package = "DSIWastewater")
    
    workset4_data <- buildWorkSheet4(wastewater_data)
    
    #Only show Site with more than 180 measurements for vignette for brevity
    workset4_data <- workset4_data[workset4_data$n >= 180,]
    workset4_data <- workset4_data[workset4_data$WWTP == "Madison MSD WWTF",]
    workset4_Smooth_data <- do.call(rbind,
                                    lapply(
                                      split(workset4_data,~WWTP),
                                      LoessSmoothMod))
    return(workset4_Smooth_data)
  })
  
  tweekNewVarsData <- reactive({
    workset4_Smooth_data <- getData()%>%
      ExpSmoothMod(alpha = input$EXP_Alpha, beta=input$EXP_Beta)%>%
      arrange(date)%>%
      mutate(SevSmooth = rollmean(sars_cov2_adj_load_log10, k=input$AVG_Days, 
                                  fill=NA, align="right"))
    return(workset4_Smooth_data)
  })
  getRegTable <- reactive({
    reg_estimates_data <- buildRegressionEstimateTable(tweekNewVarsData(),
                                                       RunOn = c("sars_cov2_adj_load_log10",
                                                                 "SevSmooth",
                                                                 "EXP",
                                                                 "Loess"))
  })

    output$distPlot <- renderPlot({
        plt <- createDHSMethod_Plot(getRegTable(), tweekNewVarsData(), 
                             PointVal = c( "sars_cov2_adj_load_log10"),
                             LineVal = c("Loess","EXP", "SevSmooth"))
        return(plt)

    })

})



ParameterGuess <- function(DF,InVar, Base, max){
  temp <- DF%>%
    filter(!is.na((!!sym(InVar))))%>%
    summarise(n=n())
  span <- min(c(Base/temp$n, max))#More can be done here
  return(span)
}

ExpSmoothMod <- function(DF,InVar="sars_cov2_adj_load_log10",
                         OutVar="EXP",alpha="guess",beta="guess",
                         Filter = NULL ){
  
  if(alpha=="guess"){
    alpha <- ParameterGuess(DF,InVar, 35.6, .4)
  }
  if(beta=="guess"){
    beta <- ParameterGuess(DF,InVar, 8.9, .4)
  }
  if(!is.null(Filter)){
    OutDF <- DF%>%
      filter((!!sym(Filter)) | is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!(!!sym(Filter))  & !is.na(!!sym(InVar)))
  }else{
    OutDF <- DF%>%
      filter(is.na(!!sym(InVar)))%>%
      mutate(!!OutVar := NA)
    
    DF <- DF%>%
      filter(!is.na(!!sym(InVar)))
  }
  
  DF <- DF%>%
    arrange(date)
  
  DF[[OutVar]] <- robets::robets(y=DF[[InVar]],
                                 model = "AAN",
                                 beta  = beta,
                                 alpha=alpha)$fitted#2 iterations remove some bad patterns
  DF <- DF%>%
    bind_rows(OutDF)
  
  return(DF)
}