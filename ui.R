library(shiny)
library(mine)
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
      
      textInput(inputId="txt_search", HTML("<div style=\"font-weight: bold\">Search string</div>"), "(bacterial vaginosis)NOT(probiotic)"),
      tags$style(type='text/css', "#txt_search { width: 500px; }"),
      textInput(inputId="txt_lim", HTML("<div style=\"font-weight: bold\">Limit</div>"), "100"),
      tags$style(type='text/css', "#txt_lim { width: 50px; }"),
      br(),
      
      textInput(inputId="kterms1", HTML("<div style=\"font-weight: bold\">Key terms, Set #1</div>"), "bacterial vaginosis,BV,vaginal bacteriosis, bacteriosis"),
      helpText("Provide a set of words (key terms) that describe the area of research #1"),
      tags$style(type='text/css', "#kterms1 { width: 500px; }"),
      br(),
      textInput(inputId="kterms2", HTML("<div style=\"font-weight: bold\">Key terms, Set #2</div>"), "yeast infection, Candidiasis,fungal infection"),
      helpText("Provide a set of words (key terms) that describe the area of research #2"),
      tags$style(type='text/css', "#kterms2 { width: 500px; }"),
      br(),
      textInput(inputId="kterms3", HTML("<div style=\"font-weight: bold\">Key terms, Set #3</div>"), "preterm birth, partus praetemporaneus, partus praematurus, Premature birth, preemies,premmies"),
      helpText("Provide a set of words (key terms) that describe the area of research #3"),
      tags$style(type='text/css', "#kterms3 { width: 500px; }"),
      #br(),
      #textInput(inputId="kterms4", HTML("<div style=\"font-weight: bold\">Key terms, Set #4</div>")),
      #helpText("Provide a set of words (key terms) that describe the area of research #4"),
      #tags$style(type='text/css', "#kterms4 { width: 500px; }"),
      #br(),
      #textInput(inputId="kterms5", HTML("<div style=\"font-weight: bold\">Key terms, Set #5</div>")),
      #helpText("Provide a set of words (key terms) that describe the area of research #5"),
      #tags$style(type='text/css', "#kterms5 { width: 500px; }"),
      br(),
      
      textInput(inputId="gsize", HTML("<div style=\"font-weight: bold\">Minimum group size</div>"), "5"),
      tags$style(type='text/css', "#gsize { width: 50px; }"),
      br(),
      
      textInput(inputId="from_date", HTML("<div style=\"font-weight: bold\">From</div>"), "2010"),
      tags$style(type='text/css', "#from_date { width: 50px; }"),
      textInput(inputId="to_date", HTML("<div style=\"font-weight: bold\">To</div>"), tail(strsplit(date(),' ')[[1]],1) ),
      tags$style(type='text/css', "#to_date { width: 50px; }"),
      br(),
      
      plotOutput("balmPlotLegend", height = "100px"),

      br(),
      helpText("Provide a list of species (i.e. bacteria names) or other words of your interest to find them in texts:"),
      tags$textarea(id="textarea.in", rows=12, "L. iners\nL. crispatus\nL. gasseri\nL. jensenii\nL. acidophilus\nL. reuteri\nL. casei\nL. buchneri\nL. fermentum\nL. rhamnosus\nL. brevis"),#fileInput(inputId = "file1", label="Upload set of words:",accept=c('text/csv', 'text/comma-separated-values,text/plain')),
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
      #h4("Network"),
      #plotOutput("balmPlot", height="1280px"),
      #h4("Information"),
      #plotOutput("balmPlotPie", height = "1000px")
      tabsetPanel(
        tabPanel("Plot", h4("Network"), plotOutput("balmPlot",height="1280px"), h4("Information"), plotOutput("balmPlotPie", height = "1000px")), 
        tabPanel("Report", 
                 tableOutput("freq_table"),  
                 downloadButton('downloadReport', 'Download report')
                 
                 ) 
      )
    )
    
  )
)