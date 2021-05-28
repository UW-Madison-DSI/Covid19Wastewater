library(shiny)
library(ggpubr)
library(dplyr)
library(shinydashboard)
library(shinyjqui)
library(shinycssloaders)
source("../Scripts/GenPlotMaking.R")
source("../Scripts/WasteWaterDataProccess.R")
source("../Scripts/CassesDataProccess.R")
source("../Scripts/HelperFunctions.R")
LatWasteFN <- "../../UntrackedData/WW SARS-COV-2 Data V5.xlsx"
LatSpringCaseFN="../../UntrackedData/SpringSemester_CasesByDorm.tsv"
LatFallCaseFN="../../UntrackedData/Oct2020toEndFall_CasesByDorm.tsv"
LatMMSDFN = "../../UntrackedData/MMSD_Cases.csv"


HFGWasteFN = "../../UntrackedData/HFG data for stats preliminary 3-18-21.xlsx"
HFGCaseFN="../../UntrackedData/Madison_HighFreq_CaseData_20210507.csv"

#MMSD Data

LatCaseDormsDF=CovidDataDorms(LatSpringCaseFN,LatFallCaseFN)%>%
  mutate(Per_pos=100*Cases/Tests)

LatCaseMMSDData=CovidData(LatMMSDFN)%>%
  mutate(Per_pos=100*Cases/Tests)%>%
  select(Date,Site,Cases,Tests,Per_pos)

LatCaseDF=rbind(LatCaseMMSDData,LatCaseDormsDF)
LatCaseDF=RollPerPos(LatCaseDF,"Cases","Tests",Fucet="Site")%>%
  filter(!is.na(Site))%>%
  select(Date,Site,Cases,Tests,Per_pos,rollingPer_pos,SevCases)


LatWasteDF=WasteWater(LatWasteFN)%>%
  mutate(Date=as.Date(Date))%>%
  filter(!is.na(Date),!is.na(N1))%>%
  filter(Date<mdy("6/5/2021"))%>%
  select(Date,Site,N1,N2,PMMoV,Pct_BCoV)


#HFG data

#Reads Transformed HFG Data
HFGWasteDF=HFGInfo(HFGWasteFN)%>%
  select(Date,Site=Plant,N1=N1GC,N2=N2GC,PMMoV=PMMOVGC,Pct_BCoV=BCoV)%>%
  mutate(Site=ifelse(Site!="Madison",NA,"MadisonHFG"))


HFGCaseDF=full_join(rename(HFGCasesPARSER(HFGCaseFN),Site=Plant),filter(HFGWasteDF,Site!="MadisonHFG"),by=c("Site","Date"))%>%
  mutate(Site=ifelse(Site!="Madison",NA,"MadisonHFG"))

Fullday=data.frame(Date=seq.Date(min(HFGCaseDF$Date),max(HFGCaseDF$Date),1))
HFGCaseDF=full_join(HFGCaseDF,Fullday,by=c("Date"))%>%
  arrange(Site,Date)%>%
  group_by(Site)%>%
  mutate(SevCases=RollAvg(cases))%>%
  ungroup()%>%
  select(Date,Site,Cases=cases,SevCases)%>%
  mutate(Tests=NA,rollingPer_pos=NA)


HFGWasteDFTab2=HFGInfo(HFGWasteFN)%>%
  select(Date,Site=Plant,N1=N1GC,N2=N2GC,PMMoV=PMMOVGC,Pct_BCoV=BCoV)%>%
  mutate(Site=ifelse(Site!="Madison",Site,"MadisonHFG"))

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
                                              choices = c(unique(HFGWasteDF$Site),unique(LatWasteDF$Site)),
                                              multiple=T,
                                              selected=c(unique(LatWasteDF$Site))),
                                  sliderInput("offsetlimits", label = "lag Range", min = -21, 
                                              max = 21, value = c(-7, 7)),
                                  textInput(inputId="Formula", label="Correlation Formula", value = "Cases~log(N1)/PMMoV^2"),
                                  inputOptions
               )),
               jqui_resizable(box(width=7,title ="Corralation given location and time Lag",br(),Plot1, br(),BotDisc,BotDisc2))
             ))

Plot2=div(withSpinner(plotOutput("plot2",inline=TRUE)),style="overflow-x: scroll")
Tab2=tabItem(tabName = "BoxPlot",
             # Boxes need to be put in a row (or column)
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site2", label = ("Site"),
                                              choices = c("MMSDData","HFGData"),
                                              multiple=F,
                                              selected="MMSDData"),
                                  sliderInput("BinSiz", label = "Bin Size", min = 7, 
                                              max = 30, value = 30),
                                  selectInput(inputId = "testComp",label = "Cases data",
                                              choices=c("None","Percent Positive","Number of positive tests"),
                                              selected="None")
                                              )),
               jqui_resizable(box(width=7,title ="boxplot of concentrations",br(),Plot2, br(),BotDisc,BotDisc2))
             ))



#Creates UI for dashboard
ui <- dashboardPage(
  #top left descriptor
  dashboardHeader(title = "Waste Water interactive"),
  #left hand side tab selector
  dashboardSidebar(sidebarMenu(
    menuItem("Cor Matrix", tabName = "CorMat", icon = icon("dashboard")),
    menuItem("BoxPlot Graphic", tabName = "BoxPlot", icon = icon("dashboard"))
  )),
  #Contents of different tabs defined above
  dashboardBody(
    tabItems(
      Tab1,
      Tab2
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
    return(filter(HFGWasteDF, HFGWasteDF$Site %in% input$Site))
  })
  FullWasteData = reactive({
    return(rbind (HFGWasteSubDF(),MMSDWasteSubDF())%>%
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
  MMSDWasteSubDF2<- reactive({
    if ("MMSDData" %in% input$Site2){
      return(LatWasteDF)
    }
    return(filter(LatWasteDF,LatWasteDF$Site %in% input$Site2))
  })

  HFGWasteSubDF2<- reactive({
    if ("HFGData" %in% input$Site2){
      return(HFGWasteDFTab2)
    }
    return(filter(HFGWasteDFTab2,HFGWasteDFTab2$Site %in% input$Site2))
  })

  FullWasteData2 = reactive({
    return(rbind(HFGWasteSubDF2(),MMSDWasteSubDF2())%>%
             filter(!is.na(Site)))
  })
  MMSDCaseSubDF2<- reactive({
    if ("MMSDData" %in% input$Site2){
      return(LatCaseDF)
    }
    return(filter(LatCaseDF,LatCaseDF$Site %in% input$Site2))
  })
  HFGCaseSubDF2 = reactive({
    if ("HFGData" %in% input$Site2){
      return(HFGCaseDF)
    }
    return(filter(HFGCaseDF,HFGCaseDF$Site %in% input$Site2))
  })
  FullCasesData2 = reactive({
    return(rbind(HFGCaseSubDF2(),MMSDCaseSubDF2())%>%
             filter(!is.na(Site)))
  })  

  #unique(HFGWasteDFTab2$Site),unique(LatWasteDF$Site)
  
  output$plot2<-renderPlot(
    width = function() 300+500*length(input$Site2)*8,
    height = function() 800+500*0,{
      boxGraphic=BoxPlotProduction(FullWasteData2(),"Date","N1","Site",BinSiz=input$BinSiz)+Second_theme(ConfigOption())
      
      if(input$testComp=="Percent Positive"){
        TestType="Per_pos"
        secTestType="rollingPer_pos"
      }else if(input$testComp=="Number of positive tests"){
        TestType="Cases"
        secTestType="SevCases"
      }else{
        return(boxGraphic)
      }
      casePlot=Buildplot_gen(TestType,MainDF=FullCasesData2(),
        LineVari=secTestType,Standards=ConfigOption(),Loc="Site",
        DateLimits=c(min(FullWasteData2()$Date),max(FullWasteData2()$Date)),
        AxisPos="bottom",Xfreq="31 days")+
        Header_theme(ConfigOption())
      return(ggarrange(plotlist=list(casePlot,boxGraphic),ncol =1))
    })
}
shinyApp(ui = ui, server = server)

