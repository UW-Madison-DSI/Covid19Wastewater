
#constants and presets
maxLeftShift=-14
maxRightShift=14


#Parts Of Tab 1 defined outside for ease of use
TopDisc=div("")
BotDisc=div("Data source: Case Data from PnC Oct2020toEndFall_CasesByDorm and SpringSemester_CasesByDorm")
BotDisc2=div("Wastewater data from the DHS: WW SARS-COV-2 Data V5")
BotDisc3=div("Questions? Contact Marlin Lee mrlee6@wisc.edu or Steve Goldstein sgoldstein@wisc.edu")
Plot1=div(withSpinner(plotOutput("plot1",inline=TRUE)),style="overflow-x: scroll")
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
               jqui_resizable(box(width=7,TopDisc,br(),Plot1, br(),BotDisc,BotDisc2,BotDisc3))
             ))


BotDisc=div("HFG Waste water data source: HFG data for stats preliminary 3-18-21.xlsx from 3/18/2021 jocelyn.hemming@slh.wisc.edu email.")
BotDisc1.5=div("longitudinal Wastewater data from the DHS: WW SARS-COV-2 Data V5")
BotDisc1.75=div("Dorm Case Data from PnC Oct2020toEndFall_CasesByDorm and SpringSemester_CasesByDorm")
BotDisc2=div("Questions? Contact Marlin Lee mrlee6@wisc.edu or Steve Goldstein sgoldstein@wisc.edu")



#Parts Of Tab 2 defined outside for ease of use
TopDisc=div("Pink sections are weekends")
Plot2=div(withSpinner(plotOutput("plot2",inline=TRUE)),style="overflow-x: scroll")
#Creates buttons for Site location,Line, and scale
Tab2=tabItem(tabName = "HFGDash",
             # Boxes need to be put in a row (or column)
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site2", label = ("Site"),
                                              choices = unique(HFGFrame$Site),
                                              multiple=T,
                                              selected=c("Madison","Kenosha","Oshkosh","Wausau")),
                                  selectInput(inputId = "Vars2Waste", label = ("Waste Water Varibles"),
                                              choices = c("N1","N2","PMMoV","Pct_BCoV"),
                                              multiple=T,
                                              selected=c("N1")),
                                  selectInput(inputId = "Vars2Cas", label = ("Cases Varibles"),
                                              choices = c("EpisodeCases","CollectedCases","ConfirmedCases"),
                                              multiple=T,
                                              selected=c("CollectedCases")),
                                  conditionalPanel(
                                    condition = "input.VarsE2.length!=0",
                                    checkboxInput(inputId="7day2",label=("7 day average of cases"),value=T),
                                    checkboxInput(inputId="PopNorm",label=("Normalize by population"),value=T),
                                    sliderInput("Offset2", "Shift cases Date",min = maxLeftShift, max = maxRightShift,step=1,value=0)
                                  ),
                                  conditionalPanel(
                                    condition = "input.Vars2Cas.length!=0",
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
               jqui_resizable(box(width=7,title ="High frequency waste water analysis",TopDisc,br(),Plot2, br(),BotDisc,BotDisc2))
             ))

Tab3=tabItem(tabName = "ThreshDash",
             fluidRow(
               jqui_resizable(box(width=4,
                                  title = "Controls",
                                  selectInput(inputId = "Site3", label = ("Site"),
                                              choices = unique(c(unique(HFGFrame$Site),unique(LatCaseDF$Site))),
                                              multiple=T,)))))
