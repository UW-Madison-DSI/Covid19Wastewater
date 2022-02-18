
Plot1 = div(plotlyOutput("plot1"))
Tab1=tabItem(tabName = "MadDash",
             # Boxes need to be put in a row (or column)
             fluidRow(
               box(width=4,
                                  title = "tuning",
                              selectInput("Site", 
                               "Site analysed", 
                               unique(FullDF$Site), 
                               selected = "Madison"),
                                  selectInput("CaseSignal", 
                                              "Case data used", 
                                              c("EpisodeCase","SpecCollectedCase","FirstConfirmed"), 
                                              selected = "EpisodeCase"),
                                  selectInput("WasteWaterSignal", 
                                                "Waste Water Signal", 
                               choices = c("N1","N2","AVG"), 
                               selected = "N1"),
                                  checkboxInput("Cases","Case plot",value=T),
                                  checkboxInput("N1","N1 plot",value=T),
                                  checkboxInput("VarianceOutliers","Remove outliers deivance",value=F),
                                  conditionalPanel(
                                    condition = "input.VarianceOutliers == true",
                                    sliderInput("Error", "Threshhold to detect",min = 1, max = 10,step=.5,value=2.5),
                                    sliderInput("TrendSmooth", "Days smoothed for trend",min = 5, max = 30,step=1,value=20)
                                  ),
                   conditionalPanel(
                     condition = "input.VarianceOutliers == true||input.OutliersDate == true",
                     checkboxInput("ShowOutliers","Show Removed Outliers",value=F)
                     ),
                                  checkboxInput("SLD","Sheding lag distribution Cases",value=F),
                                  conditionalPanel(
                                    condition = "input.SLD == true",
                                    sliderInput("Mean", "Mean of SLD",min = 1, max = 100,step=.01,value=11.73),
                                    sliderInput("SD", "Standard Deviation of SD",min = 1, max = 100,step=.01,value=7.68),
                                    sliderInput("SLDWidth", "Convolution Width",min = 1, max = 30,step=1,value=21)
                                  ),
                                  checkboxInput("Loess","Loess N1 Smoothing",value=F),
                                  conditionalPanel(
                                      condition = "input.Loess == true",
                                      sliderInput("span", "Span variable of loess smoothing",min = .1, max = .5,step=.01,value=.25)
                                    ),
                                  checkboxInput("LogScale","Log Scale",value=F)
                                
               ),
               #title = "UW dorms Covid Visualization"
               box(Plot1)
             ))



#Creates UI for dashboard
UI  <- dashboardPage(
  #top left descriptor
  dashboardHeader(title = "Waste Water MainStory interactive"),
  #left hand side tab selector
  dashboardSidebar(sidebarMenu(
    menuItem("Madison WasteWater", tabName = "MadDash")
  )),
  #Contents of different tabs defined above
  dashboardBody(
    tabItems(
      Tab1
    ))
)
