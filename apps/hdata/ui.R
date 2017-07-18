library(shiny)
library(rticks)
# Define UI for dataset viewer application
shinyUI(fluidPage(
  titlePanel("HistoricalData"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbols", "symbols:", "OZB.CBOT"),
      textInput("start", "start:", "2011-01-01"),
      textInput("stop", "stop:", "2018-01-01")
    ),
    mainPanel(
      renderTable('holes')
    )
  )
))