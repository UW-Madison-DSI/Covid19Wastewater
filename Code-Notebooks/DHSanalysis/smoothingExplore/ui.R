

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("DHSSmoothing"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("EXP_Alpha",
                        "EXP_Alpha",
                        min = 0,
                        max = .2,
                        value = .0712),
            sliderInput("EXP_Beta",
                        "EXP_Beta",
                        min = 0,
                        max = .2,
                        value = .0178)
            ,
            sliderInput("AVG_Days",
                        "Days Smoothed",
                        min = 1,
                        max = 20,
                        value = 7)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
