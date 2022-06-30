
UI <- function(){
  
  Plot1 = div(plotlyOutput("plot1"))
  Plot2 = div(plotOutput("plotCSV"))
  #Creates UI for dashboard
  
  Tab1=tabItem(tabName = "MadDash",
               # Boxes need to be put in a row (or column)
               fluidRow(
                 box(width=4,
                     title = "parameters",

                     selectInput("WastewaterSignal", 
                                 "Waste Water Signal", 
                                 choices = c("N1","N2"), 
                                 selected = "N1"),
                     
                     checkboxInput("isOutlier", 
                                 "Remove Outlier",
                                 value=T),
                     checkboxInput("isLog", 
                                   "Log conc",
                                   value=F),
                     selectInput("WastewaterNorm", 
                                 "Normalization", 
                                 choices = c("No Norm","Flow", "FlowPop"), 
                                 selected = "No Norm"),
                     sliderInput("span", "Span variable of loess smoothing",min = .05, max = .5,step=.001,value=.25)
                     
                 ),
            
                 #title = "UW dorms Covid Visualization"
                 box(width=8,Plot1),
                 box(width=8,Plot2)
                 
               ))
  
  UI  <- dashboardPage(
    #top left descriptor
    dashboardHeader(title = "Waste Water loess interactive"),
    #left hand side tab selector
    dashboardSidebar(sidebarMenu(
      menuItem("Madison Wastewater", tabName = "MadDash")
    )),
    #Contents of different tabs defined above
    dashboardBody(
      tabItems(
        Tab1
      ))
  )
  return(UI)
}