library(shiny)

# Function shinyUI() determines user interface
#display.size <- system("xdpyinfo | grep dimensions", intern = TRUE) 
#d.size <- unlist(strsplit(display.size, split = "[[:space:]|(|)|x]")) 
#h.pix <- as.numeric(d.size[7]) 
#v.pix <- as.numeric(d.size[8]) 

shinyUI(
  pageWithSidebar(
    # Название приложения
    headerPanel(
      "CoPubNet"
    ),
    # Функция, определяющая структуру боковой панели приложения:
    # Боковая панель с выпадающим меню:
    # Create a sidebar on the left
    
    sidebarPanel(
      
      textInput(inputId="txt_search", "Search string", "ph[and]vagina[and]bacteria"),
      
      textInput(inputId="txt_lim", "Limit", "100"),
      
      textInput(inputId="kterms", "Key terms", "bacterial vaginosis, yeast infection, preterm birth"),
      
      br(),
      
      textInput(inputId="gsize", "Minimum group size", "5"),
      
      
      plotOutput("balmPlotLegend", height = "100px")
      
      ),
    
    mainPanel(
      h4("Abstracts found:"),
      verbatimTextOutput("total_abstracts"),
      h4("Network"),
      plotOutput("balmPlot", height="1280px"),
      h4("Information"),
      plotOutput("balmPlotPie", height = "1000px")
    )
    
  )
)