# Tabs for Endurance Limit App

# Tab: Input ----
TabInput <- 
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
  )


# Tab: Result ----
TabResult <- 
  tabPanel("Result", 
           # sidebarPanel - for Input
           sidebarPanel(
             # Title
             h3("Settings"),
             
             # Input: Run-out level (NO cycles)
             textInput("RO", 
                       label = "Run-out level (NO cycles)", 
                       value = "2e6"),
             
             # Input: Unit
             textInput("unit", 
                       label = "Unit", 
                       value = "MPa"),
             
             # Input: Confidence level [%]
             numericInput("conf.level", 
                          label = "Confidence level [%]", 
                          value = 95),
             
             hr(),
             
             # Input: Title, Ylabel & Xlabel
             textInput("title", label = "Title", 
                       value = ""),  
             textInput("ylab", label = "Ylabel", 
                       value = "Load level"),  
             textInput("xlab", label = "Xlabel", 
                       value = "Specimen number"),
             
             hr(),
             
             # Submitt button: Update
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




