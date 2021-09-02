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
  select(Date,Site, BCoV, N1,N2)

WeightVec <- unlist(lapply((split(dgamma(1:21,scale =5.028338,shape =2.332779),c(rep(1,7),rep(2,7),rep(3,7))
              )),mean))

MergedDF <- BestCorDFGen(FullCase,LIMSFullDF,"Madison",keep=c("N1","N2"))



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
      checkboxInput(inputId = "Inter",
                    label = "Model with intercept"),
      selectInput(inputId = "MatType",
                  label = "Heatmap value",
                  choices = c("R2","PVal","COR"),
                  selected = "R2"),
      selectInput(inputId = "CaseType",
                  label = "Case used",
                  choices = c("BinningThenSLDCases","SLDThenBinningCases","BinningCases"),
                  selected = "BinningThenSLDCases"),
      selectInput(inputId = "N1Type",
                  label = "Covid-19 Signal used",
                  choices = c("N1Mean","N1Median","AVGMean","AVGMedian"),
                  selected = "N1Median"),
      dateRangeInput("UsedData",
                     "Fit Data",
                     start=min(MergedDF$Date),
                     end=max(MergedDF$Date),
                     min=min(MergedDF$Date),
                     max=max(MergedDF$Date),
                     format="mm-dd-yyyy")

    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("HeatMap", plotOutput("Matrix")),
        tabPanel("Table",
                 downloadButton("TabDown", 
                                label ="DownLoad Table"),
                 tableOutput('Table')),
        tabPanel("Plot", plotOutput("Plot")),
        tabPanel("Plot2", plotOutput("Plot2")))
    )
  )
)



WasteServer <- function(input, output, session) {

  MainDF <- reactive({
    MergedDF%>%
      filter(Date>=input$UsedData[[1]],
             Date<=input$UsedData[[2]])
  })
  
  HeatmapData <- reactive({
    CheckFunction(DF=MainDF(),
                  WeightVec,
                  DaySmoothing=c(7,14),
                  Mat=TRUE,
                  Ret=input$MatType,
                  CasesUsed=input$CaseType,
                  NSUsed=input$N1Type,
                  LogModel=input$Log,
                  Intercept=input$Inter)[[1]]
  })
  ModelDataTable <- reactive({
    if(input$UsedData[1]==min(MergedDF$Date)&&
       input$UsedData[2]==max(MergedDF$Date)){
      PreDoneDF <- read.csv("BinnedModel-2021-09-01.csv")
      return(PreDoneDF)
    }
    N1Options <- c("N1Mean","N1Median","AVGMean","AVGMedian")
    do.call("rbind",lapply(N1Options,TableDFGen2,DF=MainDF()))
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
    PlotingOptions(DF=MainDF(),
                   Weights=WeightVec,
                   StartDate=as.numeric(input$Start),
                   DaySmoothing=as.numeric(input$BinSize),
                   Lag=as.numeric(input$Lag),
                   CasesUsed=input$CaseType,
                   NSUsed=input$N1Type,
                   Ret="Plot",
                   LogModel=input$Log,
                   Show=TRUE,
                   Intercept=input$Inter)[[1]]
  })
  output$Plot2<-renderPlot(width=700,height=600,{
    PlotingOptions(DF=MainDF(),
                   Weights=WeightVec,
                   StartDate=as.numeric(input$Start),
                   DaySmoothing=as.numeric(input$BinSize),
                   Lag=as.numeric(input$Lag),
                   CasesUsed=input$CaseType,
                   NSUsed=input$N1Type,
                   Ret="Plot",
                   LogModel=input$Log,
                   Show=TRUE,
                   Intercept=input$Inter)[[2]]
  })
  output$Table <-  renderTable({
    ModelDataTable()
  },digits=-2)
  
  output$TabDown <- downloadHandler(
    filename = function() {
      paste("BinnedModel-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      data.table::fwrite(ModelDataTable(), file)
    }
  )
}
shinyApp(ui = WasteUI, server = WasteServer)
