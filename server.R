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
    
    #Construct key terms
    kterms <- list()
    
    if(nchar(input$kterms1) > 0 ) {
      ans <- strsplit(toString(input$kterms1), ',')[[1]]
      #print( ans[1])
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
      
    if(nchar(input$kterms2) > 0 ) {
      ans <- strsplit(toString(input$kterms2), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    if(nchar(input$kterms3) > 0 ) {
      ans <- strsplit(toString(input$kterms3), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      print(nchar(input$kterms3)[1])
      kterms[[ ans[1] ]] <- ans
    }  
    
    if(nchar(input$kterms4) > 0 ) {
      ans <- strsplit(toString(input$kterms4), ',')[[1]]
      #print(width(input$kterms4))
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }  
    
    if(nchar(input$kterms5) > 0 ) {
      ans <- strsplit(toString(input$kterms5), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    kterms <- kterms[!is.na(kterms)]
    print(kterms)
    #data <- mine(input$txt_search,input$txt_lim,c(strsplit(toString(input$kterms),split=','))[[1]],toString(input$from_date),toString(input$to_date))
    data <- mine(input$txt_search,as.numeric(input$txt_lim),kterms,toString(input$from_date),toString(input$to_date))
    data
    
  }
  
  output$balmPlot <- renderPlot(
  {  
    cat('output$balmPlot was called\n')
    print(input$gsize)
    tryCatch(balm.vizualize2(values$data$data_table, input$gsize), error=function(e) e)
  })
  
  output$total_abstracts <- renderText({
    #cc <- "In progress"
    print(input$txt_search)
    print(input$txt_lim)
    print(input$kterms1)
    
    updateData() #Here we are downloading abstracts is any of the values above has changed
    values$data$num_records
  })
  
  output$intense <- reactivePrint(function() {
    if(input$gsize==2){
      Sys.sleep(10)
      return('Finished')
    }else({return(NULL)})
  })
  
  output$balmPlotLegend = renderPlot(
  {
    cat('output$balmPlotLegend was called\n')
    #Construct key terms
    kterms <- list()
    
    if(nchar(input$kterms1) > 0 ) {
      ans <- strsplit(toString(input$kterms1), ',')[[1]]
      #print( ans[1])
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    if(nchar(input$kterms2) > 0 ) {
      ans <- strsplit(toString(input$kterms2), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    if(nchar(input$kterms3) > 0 ) {
      ans <- strsplit(toString(input$kterms3), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }  
    
    if(nchar(input$kterms4) > 0 ) {
      ans <- strsplit(toString(input$kterms4), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }  
    
    if(nchar(input$kterms5) > 0 ) {
      ans <- strsplit(toString(input$kterms5), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    kterms <- kterms[!is.na(kterms)]
    balm.legend(c(names(kterms),"other"))
  })
  
  output$balmPlotPie = renderPlot(
  { 
    cat('output$balmPlotPie was called\n')
    print(input$gsize)
    print(input$kterms1)
    
    #Construct key terms
    kterms <-list()
    
    if(nchar(input$kterms1) > 0 ) {
      ans <- strsplit(toString(input$kterms1), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    if(nchar(input$kterms2) > 0 ) {
      ans <- strsplit(toString(input$kterms2), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }
    
    if(nchar(input$kterms3) > 0 ) {
      ans <- strsplit(toString(input$kterms3), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }  
    
    if(nchar(input$kterms4) > 0 ) {
      ans <- strsplit(toString(input$kterms4), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }  
    
    if(nchar(input$kterms5) > 0 ) {
      ans <- strsplit(toString(input$kterms5), ',')[[1]]
      #kterms[[ gsub(" ","",ans[1], fixed=T) ]] <- ans
      kterms[[ ans[1] ]] <- ans
    }  
    
    kterms <- kterms[!is.na(kterms)]
    #print(kterms)
    
    
    
    #tryCatch(balm.pie2( values$data$data_table, input$gsize, c(c(strsplit(toString(input$kterms),split=','))[[1]],"other"), values$data$freq_table ), error=function(e) e)
    tryCatch(balm.pie2( values$data$data_table, input$gsize, c(names(kterms),"other"), values$data$freq_table ), error=function(e) e)
    
  })
  
})
