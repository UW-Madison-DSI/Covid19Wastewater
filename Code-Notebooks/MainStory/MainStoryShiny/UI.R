
Plot1 = div(plotlyOutput("plot1"))
Tab1=tabItem(tabName = "MadDash",
             # Boxes need to be put in a row (or column)
             fluidRow(
               box(width=4,
                                  title = "tuning",
                                  conditionalPanel(
                                    condition = "1 == 0",
                                    checkboxInput("PerPos","PerPos",value=F)
                                  ),
                                  
                                  checkboxInput("Cases","Case plot",value=T),
                                  checkboxInput("N1","N1 plot",value=T),
                                  checkboxInput("LogScale","Log Scale",value=F),
                                  checkboxInput("VarianceOutliers","Remove outliers deivance",value=F),
                                  conditionalPanel(
                                    condition = "input.VarianceOutliers == true",
                                    sliderInput("Error", "Times larger then trend",min = 1, max = 10,step=1,value=5),
                                    sliderInput("TrendSmooth", "Days smoothed for trend",min = 5, max = 30,step=1,value=20)
                                  ),
                                  checkboxInput("OutliersDate","Remove early Date Outliers",value=F),
                                  conditionalPanel(
                                    condition = "input.OutliersDate == true",
                                    dateInput("OutliersDate2","Date threshold",value="2020-11-20",
                                    min = min(FullDF$Date),
                                    max = max(FullDF$Date))
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
                                  checkboxInput("MinMax","Min Max Normalized tight",value=F)
                                
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
