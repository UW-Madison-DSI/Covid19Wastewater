library(shiny)
library(ggpubr)
library(dplyr)
library(shinydashboard)
library(shinyjqui)
library(shinycssloaders)

#Data Files and prepwork
source("../../Scripts/GenPlotMaking.R")
source("../../Scripts/WasteWaterDataProccess.R")
source("../../Scripts/CassesDataProccess.R")
source("../../Scripts/HelperFunctions.R")
LatWasteFN <- "../../../UntrackedData/WW SARS-COV-2 Data V5.xlsx"
LatSpringCaseFN="../../../UntrackedData/SpringSemester_CasesByDorm.tsv"
LatFallCaseFN="../../../UntrackedData/FallSemester_CasesByDorm.tsv"
HFGWasteFN = "../../../UntrackedData/HFG data for stats preliminary 3-18-21.xlsx"
HFGCaseFN="../../../UntrackedData/HighFreq_CaseData_2021-05-07.csv"
LatMMSDFN = "../../../UntrackedData/MMSD_Cases.2021-05-21.csv"

source("../../Scripts/WasteShiny/PrepData.R", local = TRUE)

source('../../Scripts/WasteShiny/UIWaste.R', local = TRUE)

source('../../Scripts/WasteShiny/ServerWaste.R', local = TRUE)



shinyApp(
  ui = WasteUI,
  server = WasteServer
)