library(dplyr)
library(ggplot2)
library(lmtest)
library(lubridate)
library(limma)
library(tidyr)
library(plotly)
library(gridExtra)
library(data.table)
library(formattable)

#Data Files and prep work
source("../../lib/DataProccess.R")
source("../../lib/NormFuncs.R")
library(shiny)
library(DT)

BaseDir <- "Z:/"#get the root of the directory where the data is stored

#All the Madison data is contained in these two files#MMSD_Cases_processed
MadisonCaseFN  <-  paste0(BaseDir, "COVID-19_WastewaterAnalysis/data/processed/MMSD_Cases_2022_01_12_processed.csv")
LIMSFN <- paste0(BaseDir, "COVID-19_WastewaterAnalysis/data/processed/LIMSWasteData_01-25-21_01-05-22.csv")


#Importing the Madison case data
LatCaseDF <- ParseData(MadisonCaseFN)%>% 
  filter(Site == "Madison")%>%
  mutate(SevenDayMACases = rollapply(data = Cases, width = 7, FUN = mean, 
                                     na.rm = TRUE,fill=NA))%>%
  filter(Date>ymd("2020-9-10"))%>%
  select(Date, Site, Cases,SevenDayMACases)

#Importing the Madison waste water data
LIMSFullDF <- ParseData(LIMSFN)%>%
  filter(Site == "Madison")%>%
  mutate(N1Pure = ifelse(is.na(N1),N2,N1),
         N2Pure = ifelse(is.na(N2),N1,N2),
         GeoMeanN12 = exp((log(N1Pure)+log(N2Pure))/2))%>%
  select(Date, Site, N1, BCoV , N2 , PMMoV,
         GeoMeanN12,FlowRate,temperature,equiv_sewage_amt)

#joining the two data frames together
FullDF <- full_join(LatCaseDF,LIMSFullDF, by = c("Date","Site"))


ui <- fluidPage(
  # Title 
  titlePanel(
    h1("WasteWaterData Outlier detection", style = "padding-bottom: 20px")
  ),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      selectInput("Var",
                  "Detecting Variable",
                  c("N1","N2","GeoMeanN12","PMMoV","BCoV"),
                  selected = "N1"),
      sliderInput(
        'DaySmoothed', 
        'Day Smoothed',
        min = 7,
        max = 42,
        value = 21,
        step = 1
      ),
      sliderInput(
        'IQR', 
        'IQR threshold',
        min = -1,
        max = 14,
        value = 1.5,
        step = .1
      ),
      sliderInput(
        'Median', 
        'Median threshold',
        min = -1,
        max = 18,
        value = 4,
        step = .1
      ),
      sliderInput(
        'SD', 
        'SD threshold',
        min = -1,
        max = 4,
        value = 2.5,
        step = .1
      ),
      sliderInput(
        'Robust', 
        'Robust Iterations',
        min = 1,
        max = 15,
        value = 5,
        step = 1
      ),
      selectInput("Centered",
                  "Rolling align",
                  c("left","center","right"),
                  selected = "center")
    ),
    mainPanel(
      div(plotlyOutput(outputId = "Outliers"), style = "margin-top: 50px"),
      div(DT::dataTableOutput(outputId = "dataTable"), style = "margin-top: 50px")
    )
  )
)

server <- function(input, output, session) {
  
  DF <- reactive({
    ErrorMarkedDF <- FullDF%>%
      mutate(DetectedOutlier = FALSE)
    
    if(0<input$IQR){
      IQRThresh <- OutlierDetectRobustFunc(ErrorMarkedDF[[input$Var]], IQRFunc, Gap=input$IQR, Lines = TRUE, Bin = input$DaySmoothed, align = input$Centered, n = input$Robust)
      ErrorMarkedDF$IQRLow <- IQRThresh[[2]]
      ErrorMarkedDF$IQRHigh <- IQRThresh[[3]]
      ErrorMarkedDF <- ErrorMarkedDF%>%
        mutate(DetectedOutlier = ifelse(!!sym(input$Var)>IQRHigh|!!sym(input$Var)<IQRLow,TRUE,DetectedOutlier))
    }
    if(0<input$Median){
      MedThresh <- OutlierDetectRobustFunc(ErrorMarkedDF[[input$Var]], MedianFunc, Gap = input$Median, Lines = TRUE,Bin=input$DaySmoothed,align=input$Centered, n = input$Robust)
      ErrorMarkedDF$MedLow <- MedThresh[[2]]
      ErrorMarkedDF$MedHigh <- MedThresh[[3]]
      ErrorMarkedDF <- ErrorMarkedDF%>%
        mutate(DetectedOutlier = ifelse(!!sym(input$Var)>MedHigh|!!sym(input$Var)<MedLow,TRUE,DetectedOutlier))
    }
    if(0<input$SD){
      SDThresh <- OutlierDetectRobustFunc(ErrorMarkedDF[[input$Var]], MeanSDFunc,Gap = input$SD, Lines = TRUE,Bin=input$DaySmoothed,align=input$Centered, n = input$Robust)
      ErrorMarkedDF$SDLow <- SDThresh[[2]]
      ErrorMarkedDF$SDHigh <- SDThresh[[3]]
      ErrorMarkedDF <- ErrorMarkedDF%>%
        mutate(DetectedOutlier = ifelse(!!sym(input$Var)>SDHigh|!!sym(input$Var)<SDLow,TRUE,DetectedOutlier))
    }
    
    ErrorMarkedDF <- ErrorMarkedDF%>%
      filter(!is.na(DetectedOutlier))
    
    return(ErrorMarkedDF)
  })
  
  # Filter data based on selections
  output$dataTable <- DT::renderDataTable(DT::datatable({
    DF()%>%
      filter(DetectedOutlier)
  }))
  
  # Time series for air temperatures
  output$Outliers <- renderPlotly({
    SelectedMethods <- input$Options
    MainPlot <- DF()%>%
      ggplot(aes(x=Date))+
      geom_point(aes(y=!!sym(input$Var),color=DetectedOutlier),size=1)
    if(0<input$IQR){
      MainPlot <- MainPlot+
        geom_line(aes(y=IQRHigh,color = "IQR"))+
        geom_line(aes(y=IQRLow,color = "IQR"))
    }
    if(0<input$Median){
      MainPlot <- MainPlot+
        geom_line(aes(y=MedHigh,color = "Median"))+
        geom_line(aes(y=MedLow,color = "Median"))
    }
    if(0<input$SD){
      MainPlot <- MainPlot+
        geom_line(aes(y=SDLow,color = "SD"))+
        geom_line(aes(y=SDHigh,color = "SD"))
    }
    
    
    
    ggplotly(MainPlot)
    
  })
}

shinyApp(ui = ui, server = server)