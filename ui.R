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
      
      br(),
      
      textInput(inputId="kterms1", "Key terms", "bacterial vaginosis,BV,vaginal bacteriosis, bacteriosis"),
      br(),
      textInput(inputId="kterms2", "Key terms", "yeast infection, Candidiasis,fungal infection"),
      br(),
      textInput(inputId="kterms3", "Key terms", "preterm birth, partus praetemporaneus, partus praematurus, Premature birth, preemies,premmies"),
      br(),
      textInput(inputId="kterms4", "Key terms"),
      br(),
      textInput(inputId="kterms5", "Key terms"),
      
      br(),
      
      textInput(inputId="gsize", "Minimum group size", "5"),
      
      br(),
      
      textInput(inputId="from_date", "From", "2010"),
      
      textInput(inputId="to_date", "To", tail(strsplit(date(),' ')[[1]],1) ),
      
      br(),
      
      plotOutput("balmPlotLegend", height = "100px"),

      br(),
      
      submitButton("Submit")
      
      ),
    
    mainPanel(
      h4("Abstracts found:"),
      verbatimTextOutput("total_abstracts"),
      ### show timer
      #tabsetPanel(
      #  tabPanel("Start",value=1),
      #  tabPanel("Compute", list(
      #    ### show timer
      #    #div(id='progress',includeHTML("timer.js")),
      #    ### long function
      #    verbatimTextOutput("intense")),
      #           value=2),id='panel'),
      h4("Network"),
      plotOutput("balmPlot", height="1280px"),
      h4("Information"),
      plotOutput("balmPlotPie", height = "1000px")
    )
    
  )
)