
rm(list = ls())

setwd("C:/Users/i885564/Documents/Model/K-Means")

library(readxl)
library(shiny)
library(shinydashboard)
library(dygraphs)

source("BackTesting_Func.R")
source("server.R")

MktList<-c("Corn","Wheat","KWheat","Soybean","SMeal","BOil")
AlgoList<-c("Kmeans1","Kmeans2")

checkMkt<-checkboxGroupInput("markets","choose markets",
                             choices=MktList,
                             selected=MktList)

checkStrat<-checkboxGroupInput("Algo","choose Algo",
                               choices=AlgoList,
                               selected=AlgoList)

bttnMkt<-actionLink("selectallMkt","Select All")
bttnAlgo<-actionLink("selectallAlgo","Select All")

bttnLaunch<-actionButton("LaunchAlgo","Backtest")

chList<-c(MktList,"All")


if (interactive()) {
ui<-fluidPage(
  title = "Strategy Back Test",
  # plotOutput("plot1"),
  dygraphOutput("plot1"),
  hr(),
  
  fluidRow(
    column(4,
           
          h4("Algo Params"),
          bttnMkt,
          box(checkMkt),
          
          selectInput("Algo","Select Algo",choices=AlgoList),
          numericInput("nb","Nb Cluster",min=2,max=200,value=8),
          numericInput("thresh","Entry Threshold",value=0),
          numericInput("nbPmin","Nb Min Points",value=8),
          numericInput("NbCentile","Stop Centile",value=.2),
          br(),
          actionButton("LaunchAlgo","Clustering")
    ),
    
    column(4,
          h4("Trading Params"),
          selectInput("MktDisp","Select Market",choices=chList),
          selectInput("plot_select", label = h5("Plot Style"),
                       choices = list("Equity Curve" = 1, "Market" = 2), selected = 1),
          dateRangeInput("daterange",
                        label=h5("Data Range"),
                        start="2010-01-05",
                        end=Sys.Date()),  
          
          checkboxInput("addStp", "Show Stops", 
                        value = FALSE),
          
          actionButton("DispChart","Charting"),
          br(),
          tableOutput('Perf')
    )

  )
  
      
)

shinyApp(ui, server)
}




# if (interactive()) {
#   ui<-fluidPage(
#     title = "Strategy Back Test",
#     plotOutput("plot1"),
#     hr(),
#     
#     fluidRow(
#       column(12,
#              column(4,
#                     h4("Algo Params"),
#                     column(12,
#                            bttnMkt
#                     ),
#                     
#                     box(checkMkt),
#                     
#                     selectInput("Algo","Select Algo",choices=AlgoList),
#                     br(),
#                     column(6,
#                            column(3,
#                                   numericInput("nb","Nb Cluster",min=2,max=200,value=8)
#                            ),
#                            column(3,
#                                   numericInput("thresh","Entry Threshold",value=0)
#                            ),
#                            column(3,
#                                   numericInput("nbPmin","Nb Min Points",value=8)
#                            ),
#                            column(3,
#                                   numericInput("NbCentile","Stop Centile",value=.2)
#                            )
#                     ),
#                     br()
#                     
#              ),
#              column(4,
#                     actionButton("LaunchAlgo","Clustering")  
#              ),
#              
#              
#              column(8,
#                     h4("Trading Params"),
#                     selectInput("MktDisp","Select Market",choices=chList),
#                     selectInput("plot_select", label = h5("Plot Style"),
#                                 choices = list("Equity Curve" = 1, "Market" = 2), selected = 1),
#                     column(12,
#                            dateRangeInput("daterange",
#                                           label=h5("Data Range"),
#                                           start="2010-01-05",
#                                           end=Sys.Date())      
#                     ),
#                     actionButton("DispChart","Charting"),
#                     # submitButton('Submit')
#                     
#                     column(8,
#                            tableOutput('Perf')
#                     )
#                     
#              )
#              
#       )
#     )
#     
#     
#   )
#   
#   shinyApp(ui, server)
# }







