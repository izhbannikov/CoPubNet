library(shiny)
library(igraph)


source("visualize.R", echo=T)
source("mine.R", echo=T)
# function shinyServer() determines the layout
# and updates variables
shinyServer(function(input, output) {
  # Create an environment for storing data
  values <- reactiveValues()
  
  updateData <- function() {
    values[["data"]] <- getData()
  }
  
  getData <- function() {
    cat('getData was called\n')
    data <- new.env()
    data = mine(input$txt_search,input$txt_lim,c(strsplit(toString(input$kterms),split=','))[[1]])
    data
    
  }
  
  output$balmPlot <- renderPlot(
  {  
    cat('output$balmPlot was called\n')
    print(input$gsize)
    tryCatch(balm.vizualize2(values$data$data_table, input$gsize), error=function(e) e)
  })
  
  output$total_abstracts <- renderText({
    print(input$txt_search)
    print(input$txt_lim)
    print(input$kterms)
    updateData() #Here we are downloading abstracts is any of the values above has changed
    values$data$num_records
  })
  
  output$balmPlotLegend = renderPlot(
  {
    cat('output$balmPlotLegend was called\n')
    balm.legend(c(c(strsplit(toString(input$kterms),split=','))[[1]],"other"))
  })
  
  output$balmPlotPie = renderPlot(
  { 
    cat('output$balmPlotPie was called\n')
    print(input$gsize)
    tryCatch(balm.pie2( values$data$data_table, input$gsize, c(c(strsplit(toString(input$kterms),split=','))[[1]],"other"), values$data$freq_table ), error=function(e) e)
    
  })
  
})
