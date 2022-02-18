library(shiny)
library(dplyr)
library(ggplot2)
library(lmtest)
library(lubridate)
library(limma)
library(tidyr)
library(plotly)
library(shinydashboard)
library(shinyjqui)
source("DataProccess.R")
source("OutlierDetectionFuncs.R")
source("NormFuncs.R")
#Defines useful functions and the core DF
source("PrepEnvironment.R", local = TRUE)

source('UI.R', local = TRUE)

source('Server.R', local = TRUE)

shinyApp(
  ui = UI,
  server = Server
)
