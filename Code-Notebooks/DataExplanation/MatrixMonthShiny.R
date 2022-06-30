library(shiny)
library(ggpubr)
library(dplyr)
library(shinydashboard)
library(shinyjqui)
library(shinycssloaders)

#Data Files and prepwork
source("../../Scripts/GenPlotMaking.R")
source("../../Scripts/WastewaterDataProccess.R")
source("../../Scripts/CasesDataProccess.R")
source("../../Scripts/HelperFunctions.R")
LatWasteFN <- "../../../UntrackedData/WW SARS-COV-2 Data V5.xlsx"
LatSpringCaseFN="../../../UntrackedData/SpringSemester_CasesByDorm.tsv"
LatFallCaseFN="../../../UntrackedData/FallSemester_CasesByDorm.tsv"
HFGWasteFN = "../../../UntrackedData/HFG data for stats preliminary 3-18-21.xlsx"
HFGCaseFN="../../../UntrackedData/HighFreq_CaseData_2021-05-07.csv"
LatMMSDFN = "../../../UntrackedData/MMSD_Cases.2021-05-21.csv"

source("../../Scripts/WasteShiny/PrepData.R", local = TRUE)

HFGCaseDF=HFGCaseDF%>%
  mutate(Cases=CollectedCases,Tests=NA,Per_pos=NA)%>%
  select(Date,Site,Cases,Tests,Per_pos)
LatCaseDF=LatCaseDF%>%
  select(Date,Site,Cases,Tests,Per_pos)
HFGFrame=HFGFrame%>%
  select(Date,Site,N1,N2,PMMoV,AVG,Pct_BCoV)
LatWasteDF=LatWasteDF%>%
  select(Date,Site,N1,N2,PMMoV,AVG,Pct_BCoV)



maxLeftShift=-14
maxRightShift=14


#Parts Of Tab 2 defined outside for ease of use
inputOptions=div("varables are N1,N2,PMMoV,Pct_BCoV, Cases,Tests, and rollingPer_pos")
BotDisc=div("Data source: HFG data for stats preliminary 3-18-21.xlsx from 3/18/2021 jocelyn.hemming@slh.wisc.edu email.")
BotDisc2=div("Questions? Contact Marlin Lee mrlee6@wisc.edu or Steve Goldstein sgoldstein@wisc.edu")
#Creates buttons for Site location,Line, and scale
Plot1=div(withSpinner(plotOutput("plot1",inline=TRUE)),style="overflow-x: scroll")
Tab1=tabItem(tabName = "CorMat",
             # Boxes need to be put in a row (or column)
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site", label = ("Site"),
                                              choices = c(unique(HFGFrame$Site),unique(LatWasteDF$Site)),
                                              multiple=T,
                                              selected=c(unique(LatWasteDF$Site))),
                                  sliderInput("offsetlimits", label = "lag Range", min = -21, 
                                              max = 21, value = c(-7, 7)),
                                  textInput(inputId="Formula", label="Correlation Formula", value = "Cases~N1/PMMoV"),
                                  inputOptions
               )),
               jqui_resizable(box(width=7,title ="Corralation given location and time Lag",br(),Plot1, br(),BotDisc,BotDisc2))
             ))




#Creates UI for dashboard
ui <- dashboardPage(
  #top left descriptor
  dashboardHeader(title = "Waste Water interactive"),
  #left hand side tab selector
  dashboardSidebar(sidebarMenu(
    menuItem("Cor Matrix", tabName = "CorMat", icon = icon("dashboard"))
  )),
  #Contents of different tabs defined above
  dashboardBody(
    tabItems(
      Tab1
    ))
)


#create graphic                                       
server <- function(input, output) {
  ConfigOption=reactive({
    return(list(
      myseed=1234567890,
      XAxisLabSiz=10,
      YAxisLabSiz=15,
      GenFontSiz=20,
      alphaPoint=.7,
      PointSize=2,
      alphaWeek=.2)
    )
  })
  #Required for tab 1

  MMSDCaseSubDF<- reactive({
    return(filter(LatCaseDF, Site %in% input$Site))
  })
  HFGCaseSubDF = reactive({
    return(filter(HFGCaseDF, HFGCaseDF$Site %in% input$Site))
  })
  FullCasesData = reactive({
    return(rbind (HFGCaseSubDF(),MMSDCaseSubDF())%>%
             filter(!is.na(Site)))
  })  

  MMSDWasteSubDF<- reactive({
    return(filter(LatWasteDF, LatWasteDF$Site %in% input$Site))
  })
  HFGWasteSubDF<- reactive({
    return(filter(HFGFrame, HFGFrame$Site %in% input$Site))
  })
  FullWasteData = reactive({
    return(rbind(HFGWasteSubDF(),MMSDWasteSubDF())%>%
             filter(!is.na(Site)))
  })  
  FormulaCont = reactive({
    return(input$Formula)
  })

  #plots N1GC
  output$plot1<-renderPlot(
    width = function() 800+30*length(unique(input$Site)),
    height = function() 800+300*length(input$Vars)*0,{
      ploter=SiteLagHeatMap(FullCasesData(),FullWasteData(),input$offsetlimits[1]:input$offsetlimits[2],"Site","Date",FormulaCont())
      return(ploter)
    })
}
shinyApp(ui = ui, server = server)

