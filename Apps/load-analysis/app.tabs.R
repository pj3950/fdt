# Tabs for Load Analysis App

# Tab: Input ----
TabInput <- 
  tabPanel("Input data", 
           
           # sidebarPanel: for Input ---
           sidebarPanel(
             h3("Load file"),
             fileInput("file", 
                       label = "Load signal (text file with two columns)")
             #               actionButton("LoadShortLS", "Load examlpe: Short load signal")
           ),
           
           # mainPanel - for Output
           mainPanel(
             # Title & Info
             h3("Instructions"),
             
             p(textOutput("InstText")),
             
             # Output
             conditionalPanel(condition = "output.available",
                              h3(textOutput("InputText")),
                              plotOutput("InputPlot")
             )
           )
  )



# Tab: Result ----
TabResult <- 
  tabPanel("Result", 
           # sidebarPanel - for Input
           sidebarPanel(
             h3("Settings"),
             
             numericInput("h", 
                          label = "Threshold range", 
                          value = -10),
             numericInput("b", 
                          label = "Damage exponent", 
                          value = 5),
             hr(),
             textInput("title", label = "Title", 
                       value = ""),  
             textInput("ylab", label = "Ylabel", 
                       value = "Load"),  
             textInput("xlab", label = "Xlabel", 
                       value = "Time"),
             hr(),
             checkboxInput("printTable", label="Print table", value=FALSE),
             submitButton("Update")
           ),
           
           #dataTableOutput("ResultTable")
           # mainPanel - for Output
           # Show a plot of the data and estimated endurance limit and print results
           
           mainPanel(
             h2("Result: Rainflow filtered turning points"),
             h3("Information"),
             #h2("Rainflow filtered turning points"),
             p(" Negative values of threshold range is interpreted as percentage of global load range, e.g. a value -10 gives a threshold range representing 10% of the max range of the signal."),
             
             #      downloadLink('downloadTP', 'Download rainflow filtered Turning Points.'),
             # conditionalPanel( condition = "output.nrows",
             #                   checkboxInput("headonly", "Only use first 1000 rows"))      
             
             # Output
             conditionalPanel(condition = "output.available",
                              h3(textOutput("ResultHeader3")),
                              p(textOutput("ResultText1")),
                              downloadButton('downloadTP', 'Download rainflow filtered Turning Points.'),
                              
                              h3(textOutput("ResultHeader")),
                              plotOutput("RFplot"),
                              
                              h3(textOutput("ResultHeader2")),
                              plotOutput("RFplot2"),
                              
                              h3(textOutput("ResultTableHeader")),
                              tableOutput("ResultTable")
             )
           )
  )






