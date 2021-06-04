source('../Scripts/WasteShiny/TabsWaste.R', local = TRUE)
#Creates UI for dashboard
WasteUI  <- dashboardPage(
  #top left descriptor
  dashboardHeader(title = "Waste Water interactive"),
  #left hand side tab selector
  dashboardSidebar(sidebarMenu(
    menuItem("UW Dorms WasteWater", tabName = "MadDash", icon = icon("dashboard")),
    menuItem("HFG high frequencey", tabName = "HFGDash", icon = icon("dashboard"))
  )),
  #Contents of different tabs defined above
  dashboardBody(
    tabItems(
      Tab1,
      Tab2
    ))
)