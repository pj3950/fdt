# Tab: Input ----
tabPanel("Input data", 
         
         # sidebarPanel: for Input ---
         sidebarPanel(
           h3("Input"),
           fileInput("file", 
                     label = "Fatigue data (text file with three columns)")
         ),
         
         # mainPanel - for Output
         mainPanel(
           # Title & Info
           h3("Instructions"),
           
           p(textOutput("InstText")),
           
           # Output
           h3(textOutput("InputText")),
           tableOutput("TabFLdata")
         )
),

# Tab: Result ----
tabPanel("Result", 
         # sidebarPanel - for Input
         sidebarPanel(
           h3("Settings"),
           
           textInput("RO", 
                     label = "Run-out level (NO cycles)", 
                     value = "2e6"),
           textInput("unit", 
                     label = "Unit", 
                     value = "MPa"),
           numericInput("conf.level", 
                        label = "Confidence level [%]", 
                        value = 95),
           hr(),
           textInput("title", label = "Title", 
                     value = ""),  
           textInput("ylab", label = "Ylabel", 
                     value = "Load level"),  
           textInput("xlab", label = "Xlabel", 
                     value = "Specimen number"),
           hr(),
           #       checkboxInput("printTable", label="Print table", value=FALSE),
           submitButton("Update")
         ),
         
         # mainPanel - for Output
         # Show a plot of the data and estimated endurance limit and print results
         mainPanel(
           # Title & Info
           h2("Result: Estimated endurance limit"),
           p(textOutput("InfoText1")),
           
           # Plot results
           plotOutput("FLplot"),
           
           # Present results in three paragraphs
           p(textOutput("ResultText1")),
           p(textOutput("ResultText2")),
           p(textOutput("ResultText3"))
           
         )
)