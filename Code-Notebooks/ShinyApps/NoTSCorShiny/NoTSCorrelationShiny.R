# library(ggpubr)
# library(forecast)
# library(lmtest)
# library(lubridate)
#library(limma)
library(zoo)
library(dplyr)
library(RColorBrewer)
library(shiny)

#Data Files and prep work
source("GenPlotMaking.R")
source("WasteWaterDataProccess.R")
source("CasesDataProccess.R")
source("HelperFunctions.R")

LatMMSDFN  <-  "MMSD_Cases.CSV"
LIMSFN <- "WATERMICRO_WW_COVID-2021-06-30 17 40.xlsx"

LatCaseDF <- CovidDataPARSER(
    MMSDFN = LatMMSDFN
  ) %>% 
  filter(!is.na(Site)) %>%
  select(Date,Site,Cases,Tests,Per_pos)


FullCase <- LatCaseDF

FullCaseRoll <- RollPerPos(FullCase,"Cases","Tests",Facet="Site",n = 14)

LIMSFullDF <- LIMSDataPARSER(LIMSFN)%>%
  select(Date,Site, BCoV, N1,N1Error)

WeightVec <- unlist(lapply((split(dgamma(1:21,scale =5.028338,shape =2.332779),c(rep(1,7),rep(2,7),rep(3,7))
              )),mean))

MergedDF <- BestCorDFGen(FullCase,LIMSFullDF,"Madison")



WasteUI  <-  fluidPage(
  titlePanel("No TS modeling"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "Lag",
                  label = "Cases Offset",
                  min = -2,
                  max = 2,
                  value = 0),
      sliderInput(inputId = "Start",
                  label = "Day of week start",
                  min = 0,
                  max = 7,
                  value = 0),
      selectInput(inputId = "BinSize",
                  label = "Bin Size",
                  choices = c(7,14),
                  selected = 7),
      checkboxInput(inputId = "Log",
                  label = "Log Comparison"),
      selectInput(inputId = "MatType",
                  label = "Heatmap value",
                  choices = c("R2","PVal","COR"),
                  selected = "R2"),
      selectInput(inputId = "CaseType",
                  label = "Case used",
                  choices = c("BinningThenSLDCases","SLDThenBinningCases","BinningCases"),
                  selected = "R2")

    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("HeatMap", plotOutput("Matrix")),
        tabPanel("Table", tableOutput('Table')),
        tabPanel("Plot", plotOutput("Plot")))
    )
  )
)



WasteServer <- function(input, output, session) {
  

  MainDF <- reactive({
    if(input$Log){
      FinDF <- MergedDF%>%
        mutate(Cases=log(Cases),Cases2=log(Cases2),N1=log(N1))
    }else{
      return(MergedDF)
    }
  })
  
  HeatmapData <- reactive({
    CheckFunction(DF=MainDF(),WeightVec,DaySmoothing=c(7,14), Mat=TRUE,
                  Ret=input$MatType,CasesUsed=input$CaseType)[[1]]
  })

  output$Matrix<-renderPlot(width=700,height=600,{
    #Store StartDate somewhere more changeable
    DayOfWeekData <- weekdays(seq(mdy("10/1/2020"), by=1,
                                    len=8))
    xAxisPlot <- expand.grid(c(7,14), -2:2)
    xAxisPlot <- xAxisPlot[order(xAxisPlot$Var1),]
    
    AxisPattern<- apply(xAxisPlot, 1, paste, collapse=" ")
    Site=unique(MainDF()$Site)
    
    HeatMapMaker(HeatmapData(),8,AxisPattern,
                 DayOfWeekData,Site=Site, 
                 "R2 relationship",ColorName="YlOrRd")
  })
  
  
  output$Plot<-renderPlot(width=700,height=600,{
    PlotingOptions(MainDF(),WeightVec,as.numeric(input$Start),
                   as.numeric(input$BinSize),as.numeric(input$Lag),
                   Ret="Plot",Show=TRUE)
  })
  
  output$Table <-  renderTable({
    DataTableVec <- CheckFunction(DF=MainDF(),Weights=WeightVec,
                                  DaySmoothing=c(7,14),
                            Mat=TRUE, Ret="All", 
                            CasesUsed=input$CaseType)
    FullDF <- VecToDF(DataTableVec,Val="All")%>%
      separate(All,c("COR","R2","PVal"),sep=" ")
    return(FullDF)
  })
}
shinyApp(ui = WasteUI, server = WasteServer)
