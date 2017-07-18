# server.R
options(shiny.trace=TRUE)
sink(file = "hdata.log", append = FALSE)
library(shiny)
library(dplyr)
library(rticks)
cfg.reload()


shinyServer(function(input, output) {
  holes <- reactive({data_holes()})
  
  #RA <- reactive({
  #  withProgress(message = 'GetRiskArraysMatch', value = 0.3, GetRiskArraysMatch(symbol()))
  #})
  
  output$holes <- renderTable({holes()})
  
  #output$hdataplot <- renderPlot({
  #  hdata() %>% filter(exante_id=sym()) %>% ggplot(aes(x=datetime, y=minutes_qty_actual))
  #})
    
  #output$mytabs <- renderUI({
  #  ts = tables()
  #  mytabs = lapply(ts, function(tb) {
  #    tabPanel(
  #      title=paste0(tb$month,tb$year), 
  #      renderTable({tb$table})
  #    )})
  #  do.call(tabsetPanel, mytabs)
  #})
  #output$text <- renderText(paste0("margin0=",margin0()))
  #output$portfolioRA <- renderTable(portfolioRA())
  #output$downloadData <- downloadHandler(
  #  filename = c('data.csv'),
  #  content = function(file) {
  #    write.csv(margins(), file)
  #  }
  #)
})