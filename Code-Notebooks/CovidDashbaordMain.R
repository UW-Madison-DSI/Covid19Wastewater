
  
library(shiny)
library(ggpubr)
library(dplyr)
library(shinydashboard)
library(shinyjqui)
library(shinycssloaders)

#Data Files and prepwork
source("../Scripts/GenPlotMaking.R")
source("../Scripts/WasteWaterDataProccess.R")
source("../Scripts/CassesDataProccess.R")
source("../Scripts/HelperFunctions.R")
LatWasteFN <- "../../UntrackedData/WW SARS-COV-2 Data V5.xlsx"
LatSpringCaseFN="../../UntrackedData/SpringSemester_CasesByDorm.tsv"
LatFallCaseFN="../../UntrackedData/FallSemester_CasesByDorm.tsv"
HFGWasteFN = "../../UntrackedData/HFG data for stats preliminary 3-18-21.xlsx"
HFGCaseFN="../../UntrackedData/Madison_HighFreq_CaseData_20210507.csv"
LatMMSDFN = "../../UntrackedData/MMSD_Cases.2021-05-21.csv"



#tab1 data

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
  filter(!is.na(Date),!is.na(N1),!is.na(Site))%>%
  filter(Date<mdy("6/5/2021"))%>%
  select(Date,Site,N1,N2,PMMoV,Pct_BCoV)




#tab2 data

#Reads Transformed HFG Data

HFGFrame=HFGInfo(HFGWasteFN)%>%
  select(Date,Site=Plant,Filter,Well,N1GC,N2GC,PMMOVGC)%>%
  mutate(Type="Normal")%>%
  pivot_longer(N1GC:PMMOVGC)
HFGSiteMean=HFGFrame%>%
  group_by(Date,Site,name)%>%
  mutate(value=exp(mean(log(value),na.rm = T)))%>%
  mutate(Type="Site Mean")%>%
  distinct(Date,Site,name,.keep_all=T)
HFGFilterMean=HFGFrame%>%
  group_by(Date,Site,Filter,name)%>%
  mutate(value=exp(mean(log(value),na.rm = T)))%>%
  mutate(Type="Filter Mean")%>%
  distinct(Date,Site,Filter,name,.keep_all=T)
HFGWasteDF=rbind(HFGFrame,HFGSiteMean,HFGFilterMean)%>%
  pivot_wider(names_from="name",values_from="value")%>%
  mutate(Filter=as.character(Filter),Well=as.character(Well))%>%
  mutate(Date=as.Date(Date))%>%
  na.omit()%>%
  rename(`Filter replicates`=Filter)

HFGWasteDFTab3=HFGInfo(HFGWasteFN)%>%
  select(Date,Site=Plant,N1=N1GC,N2=N2GC,PMMoV=PMMOVGC,Pct_BCoV=BCoV)

HFGCaseDF=HFGCasesPARSER(HFGCaseFN)%>%
  rename(Site=Plant,Cases=cases)%>%
  full_join(filter(HFGWasteDFTab3,Site!="Madison"),by=c("Site","Date"))

Fullday=data.frame(Date=seq.Date(min(HFGCaseDF$Date),max(HFGCaseDF$Date),1))
HFGCaseDF=full_join(HFGCaseDF,Fullday,by=c("Date"))%>%
  arrange(Site,Date)%>%
  group_by(Site)%>%
  mutate(SevCases=RollAvg(Cases))%>%
  ungroup()



#Generating the weekends starts and end dates
#TO DO:Capture the weekend if the data intersects with it
DateRangeDF=data.frame(Date=seq(min(HFGCaseDF$Date), max(HFGCaseDF$Date), "days"))%>%
  mutate(Days=weekdays(Date))%>%
  filter(Days %in% c("Sunday","Monday"))
if (DateRangeDF$Days[1]=="Monday"){
  DateRangeDF=DateRangeDF[-1,]}
if (tail(DateRangeDF$Days, n=1)=="Sunday"){
  DateRangeDF=head(DateRangeDF, -1)}
MRan=filter(DateRangeDF,Days=="Sunday")%>%
  rename(Left=Date)
SRan=filter(DateRangeDF,Days=="Monday")%>%
  rename(Right=Date)
DateRangeCDF=cbind(MRan,SRan)%>%
  select(Left,Right)



#constants and presets
maxLeftShift=-14
maxRightShift=14


BotDisc=div("HFG Waste water data source: HFG data for stats preliminary 3-18-21.xlsx from 3/18/2021 jocelyn.hemming@slh.wisc.edu email.")
BotDisc1.5=div("longitudinal Wastewater data from the DHS: WW SARS-COV-2 Data V5")
BotDisc1.75=div("Dorm Case Data from PnC Oct2020toEndFall_CasesByDorm and SpringSemester_CasesByDorm")
BotDisc2=div("Questions? Contact Marlin Lee mrlee6@wisc.edu or Steve Goldstein sgoldstein@wisc.edu")
Plot3=div(withSpinner(plotOutput("plot3",inline=TRUE)),style="overflow-x: scroll")
Tab3=tabItem(tabName = "BoxPlot",
             # Boxes need to be put in a row (or column)
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site3", label = ("Site"),
                                              choices = c("MMSDData","HFGData","DormData"),
                                              multiple=F,
                                              selected="MMSDData"),
                                  sliderInput("BinSiz", label = "Bin Size", min = 1, 
                                              max = 30, value = 30),
                                  selectInput(inputId = "testComp",label = "Cases data",
                                              choices=c("None","Number of positive tests"),
                                              selected="None")
               )),
               jqui_resizable(box(width=7,title ="boxplot of concentrations",br(),Plot3, br(),BotDisc1.5,BotDisc,BotDisc1.75,BotDisc2))
             ))



#Parts Of Tab 2 defined outside for ease of use
TopDisc=div("Pink sections are weekends")
#Creates buttons for Site location,Line, and scale
Tab2=tabItem(tabName = "HFGDash",
             # Boxes need to be put in a row (or column)
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site2", label = ("Site"),
                                              choices = c("All",unique(HFGWasteDF$Site)),
                                              multiple=F,
                                              selected="Madison"),
                                  selectInput(inputId = "Vars2", label = ("Varibles"),
                                              choices = c("N1GC","N2GC","PMMOVGC","Cases"),
                                              multiple=T,
                                              selected=c("N1GC","Cases")),
                                  conditionalPanel(
                                    condition = "input.Vars2.includes('Cases')",
                                    checkboxInput(inputId="7day2",label=("7 day average of cases"),value=T),
                                    sliderInput("Offset2", "Shift cases Date",min = maxLeftShift, max = maxRightShift,step=1,value=0)
                                  ),
                                  conditionalPanel(
                                    condition = "(!input.Vars2.includes('Cases')||1!=input.Vars2.length)&&0!=input.Vars2.length",
                                    checkboxInput(inputId = "Line2", label = ("Daily mean of 9 replicates"),
                                                  value=F),
                                    checkboxInput(inputId = "Means", label = ("Mean of qPCR replicates"),
                                                  value=F),
                                    selectInput(inputId = "scale2", label = ("y scale"),
                                                choices = c("log","linear"),
                                                selected="log"),
                                    selectInput(inputId = "Outlier", label = ("Outlier"),
                                                choices = c("Remove Outliers"),
                                                multiple=T,
                                                selected="Remove Outliers")
                                  ),
                                  
                                  
                                  
               )),
               jqui_resizable(box(width=7,title ="High frequency waste water analysis",TopDisc,br(),div(withSpinner(plotOutput("plot2",inline=TRUE,width="auto",height="auto")),style="overflow-x: scroll"), br(),BotDisc,BotDisc2))
             ))



#Parts Of Tab 1 defined outside for ease of use
TopDisc=div("")
BotDisc=div("Data source: Case Data from PnC Oct2020toEndFall_CasesByDorm and SpringSemester_CasesByDorm")
BotDisc2=div("Wastewater data from the DHS: WW SARS-COV-2 Data V5")
BotDisc3=div("Questions? Contact Marlin Lee mrlee6@wisc.edu or Steve Goldstein sgoldstein@wisc.edu")
#Creates buttons for Site location,Line, and scale
Tab1=tabItem(tabName = "MadDash",
             # Boxes need to be put in a row (or column)
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site", label = ("Site"),
                                              choices = unique(LatCaseDF$Site),
                                              multiple=T,
                                              selected=c("UW-Sellery" ,"UW-LakeShore")),
                                  selectInput(inputId = "VarsWaste", label = ("Wastewater varibles"),
                                              choices = c("N1","N2","PMMoV","Pct_BCoV","AVG"),
                                              multiple=T,
                                              selected=c("N1")),
                                  selectizeInput(inputId = "VarsTest", label = ("Tests varibles"),
                                                 choices = c("Per_pos","Cases"),
                                                 multiple=T,
                                                 options = list(maxItems = 1),
                                                 selected="Per_pos"),
                                  
                                  conditionalPanel(
                                    condition = "input.VarsTest.length!=0",
                                    checkboxInput(inputId="7day",label=("7 day average of percent positive"),value=T),
                                    sliderInput("Offset", "Shift percent positive Date",min = maxLeftShift, max = maxRightShift,step=1,value=0)
                                  ),
                                  conditionalPanel(
                                    condition = "input.VarsWaste.length!=0",
                                    checkboxInput(inputId="Line",label=("loess curve of waste water"),value=T),
                                    selectInput(inputId = "scale", label = ("y scale"),
                                                choices = c("log","linear"),
                                                selected="log"),
                                    conditionalPanel(
                                      condition = "input.Line == true",
                                      sliderInput("span", "Span variable of loess smoothing",min = .15, max = .75,step=.01,value=.42)
                                    )
                                  ),
                                  
               )),
               #title = "UW dorms Covid Visualization"
               jqui_resizable(box(width=7,TopDisc,br(),div(withSpinner(plotOutput("plot1",inline=TRUE)),style="overflow-x: scroll"), br(),BotDisc,BotDisc2,BotDisc3))
             ))




#Creates UI for dashboard
ui <- dashboardPage(
  #top left descriptor
  dashboardHeader(title = "Waste Water interactive"),
  #left hand side tab selector
  dashboardSidebar(sidebarMenu(
    menuItem("UW Dorms WasteWater", tabName = "MadDash", icon = icon("dashboard")),
    menuItem("HFG high frequencey", tabName = "HFGDash", icon = icon("dashboard")),
    menuItem("BoxPlot Graphic", tabName = "BoxPlot", icon = icon("dashboard"))
  )),
  #Contents of different tabs defined above
  dashboardBody(
    tabItems(
      Tab1,
      Tab2,
      Tab3
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
  covidnameDer=reactive({
    return(list())
  })
  #Required for tab 1
  filtered_data_PWaste<- reactive({
    return(filter(LatWasteDF, LatWasteDF$Site %in% input$Site))
  })
  filtered_data_PCov<- reactive({
    return(filter(LatCaseDF, Site %in% input$Site))
  })
  
  minCDorms=reactive({
    return(min(filtered_data_PWaste()$Date)+input$Offset)
  })
  
  maxCDorms=reactive({
    return(max(filtered_data_PWaste()$Date)+input$Offset)
  })
  #Creates list of ggplots for ggarange
  Make_Plots1 = reactive({
    Date_lim_long=c(min(filtered_data_PWaste()$Date),max(filtered_data_PWaste()$Date))
    plots=lapply(X=input$VarsWaste,FUN=Buildplot_gen,
                 MainDF=filtered_data_PWaste(),
                 Loc="Site",
                 spanN=input$span,
                 log_scale=(input$scale=="log"),
                 DateLimits=Date_lim_long,Standards=ConfigOption())
    if(length(input$VarsTest)!=0){
      if(input$VarsTest=="Per_pos"){
        LineVariF="rollingPer_pos"
        ylab="% of tests positive"
      }else{
        LineVariF="SevCases"
        ylab="number of positive tests"
      }
      if(!input$'7day'){
        LineVariF=NA
      }
      CasePlot=Buildplot_gen(
        input$VarsTest,
        MainDF=filtered_data_PCov(),
        Loc="Site",
        LineVari=LineVariF,
        DateLimits=Date_lim_long+input$Offset,
        Standards=ConfigOption(),
        AxisPos="bottom",
        YLabel=ylab)
      plots=c(list(CasePlot),plots)
    }
    #DormPlot=DormPlot+ylab("Percent Positive")+
    return(plots)
  })
  #plots N1GC
  output$plot1<-renderPlot(
    width = function() 245+425*length(input$Site),
    height = function() 60+300*(length(input$VarsTest)+length(input$VarsWaste)),
    {
      if((length(input$VarsTest)+length(input$VarsWaste))==0||length(input$Site)==0){
        return()
      }
      plots=Make_Plots1()
      #Add Site labels
      #add x axis labels
      if(length(input$VarsTest)!=0){
        T=2
        if(length(input$VarsWaste)==0){
          T=1
        }
      }else{
        T=1
      }
      plots[[1]]=plots[[1]] + Header_theme(ConfigOption())
      plots[[T]]=plots[[T]] + Second_theme(ConfigOption())
      OvarLn=length(input$VarsWaste)
      return(annotate_figure(top=text_grob("UW dorms Wastewater Surveillance",size=25),ggarrange(plotlist=plots,ncol =1,align="v",heights=c(1.2,rep(1,times = OvarLn)))))
    })
  #-------------------------
  #required for tab2
  #Filters data for right Sites and variables
  HFGCaseDataFil = reactive({
    if(input$Site2!="All")
      return(filter(HFGCaseDF, HFGCaseDF$Site %in% input$Site2))
    return(HFGCaseDF)
  })
  filtered_data_P<- reactive({
    if(input$Site2!="All")
      return(filter(HFGWasteDF, HFGWasteDF$Site %in% input$Site2))
    return(HFGWasteDF)
  })
  #creates data with original values
  filtered_data_N<- reactive({
    filtered_data_P()%>%
      filter(Type=="Normal")
  })
  
  #Data of the means of each day
  Data_mean_var = reactive({
    filtered_data_P()%>%
      filter(Type=="Site Mean")
  })
  #Data of the means of each Filter
  Filter_mean_var = reactive({
    filtered_data_P()%>%
      filter(Type=="Filter Mean")
  })
  #Putting data in reactive shell for possible additions
  DF_Week = reactive({
    return(DateRangeCDF)
  })
  minCHFG=reactive({
    return(min(HFGCaseDF$Date))
  })
  
  maxCHFG=reactive({
    return(max(HFGCaseDF$Date))
  })
  
  #Creates list of ggplots for ggarange
  Make_Plots2 = reactive({
    if(input$Means==T){
      MeanVDF=Filter_mean_var()
    }else{
      MeanVDF=NA
    }
    if(input$Line2==T){
      LineVDF=Data_mean_var()
    }
    else{
      LineVDF=NA
    }
    
    
    Date_lim_HFG=c(min(HFGCaseDataFil()$Date),max(HFGCaseDataFil()$Date))
    plots=lapply(X=input$Vars2[input$Vars2!="Cases"],FUN=Buildplot_gen,
                 MainDF=filtered_data_N(),
                 Loc="Site",
                 log_scale=(input$scale2=="log"),
                 ColorType="Filter replicates",
                 LineDF=LineVDF,
                 MeanDF=MeanVDF,
                 WeekDF=DF_Week(),
                 DateLimits=Date_lim_HFG,
                 RMOutliers=("Remove Outliers" %in% input$Outlier),
                 Standards=ConfigOption(),
                 Xfreq="14 days")
    
    if("Cases" %in% input$Vars2){
      if(input$'7day2'){
        LineVariF="SevCases"
      }
      else{
        LineVariF=NA
      }
      CasePlot=Buildplot_gen(
        "Cases",
        MainDF=HFGCaseDataFil(),
        Loc="Site",
        LineVari=LineVariF,
        WeekDF=DF_Week(),
        DateLimits=Date_lim_HFG+input$Offset2,
        Standards=ConfigOption(),
        Xfreq="14 days",
        AxisPos="bottom")
      plots=c(list(CasePlot),plots)
    }
    return(plots)
  })
  #plots N1GC
  output$plot2<-renderPlot(
    width = function() 245+800*ifelse(input$Site2=="All",10,1),
    height = function() 60+300*length(input$Vars2),
    {
      if(length(input$Vars2)==0){
        return()
      }
      plots=Make_Plots2()
      #Add Site labels
      #add x axis labels
      if("Cases" %in% input$Vars2){
        T=2
        if(length(input$Vars)==1){
          T=1
        }
      }else{
        T=1
      }
      plots[[1]]=plots[[1]] + Header_theme(ConfigOption())
      plots[[T]]=plots[[T]] + Second_theme(ConfigOption())
      return(annotate_figure( left = text_grob("(GC/L)", rot = 90,size=ConfigOption()$GenFontSiz),ggarrange(plotlist=plots,ncol =1,common.legend = TRUE,legend = "right",align="v")))
    })
  #top = text_grob("High frequency waste water analysis",size=20),  
  #-------------------------
  DormOptions=reactive({
    return(c("UW-Sellery" ,"UW-LakeShore"))
  })
  #required for tab3
  FullWasteData3 = reactive({
    if ("HFGData" %in% input$Site3){
      ReleventWasteData=HFGWasteDFTab3
    }else if("DormData" %in% input$Site3){
      ReleventWasteData=filter(LatWasteDF,Site %in% DormOptions())
    }else{
      ReleventWasteData=filter(LatWasteDF,!(Site %in% DormOptions()))
    }
    ReleventWasteData=ReleventWasteData%>%
      filter(!is.na(Site))
    return(ReleventWasteData)
  })

  FullCasesData3 = reactive({
    if ("HFGData" %in% input$Site3){
      ReleventCaseData=HFGCaseDF
    }else if("DormData" %in% input$Site3){
      ReleventCaseData=filter(LatCaseDF,Site %in% DormOptions())
      }else{
      ReleventCaseData=filter(LatCaseDF,!(Site %in% DormOptions()))
    }
    ReleventCaseData=ReleventCaseData%>%
      filter(!is.na(Site))
    return(ReleventCaseData)
  })  
  
  #unique(HFGWasteDFTab2$Site),unique(LatWasteDF$Site)
  
  output$plot3<-renderPlot(
    width = function() 100+700*ifelse(input$Site3=="MMSDData",6,ifelse(input$Site3=="DormData",2,10)),
    height = function() 800+500*0,{
      xlims=c(min(FullCasesData3()$Date),max(FullCasesData3()$Date))
      
      boxGraphic=BoxPlotProduction(FullWasteData3(),"Date","N1","Site",
                BinSiz=input$BinSiz, DateLimits=xlims)+Header_theme(ConfigOption())
      
      if(input$testComp=="Number of positive tests"){
        TestType="Cases"
        secTestType="SevCases"
      }else{
        return(boxGraphic)
      }
      
      casePlot=Buildplot_gen(TestType,MainDF=FullCasesData3(),
                             LineVari=secTestType,Standards=ConfigOption(),
                             Loc="Site", DateLimits=xlims,
                             AxisPos="bottom",Xfreq="21 days")+
        Header_theme(ConfigOption())
      boxGraphic=boxGraphic+Second_theme(ConfigOption())
      return(ggarrange(plotlist=list(casePlot,boxGraphic),ncol =1,align="v"))
    })
}
shinyApp(ui = ui, server = server)