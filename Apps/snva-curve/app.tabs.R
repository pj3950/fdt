DeltaS <- "\u0394S"
#DeltaS <- "S"

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
             h3("Fatigue test data"),
             
             p(textOutput("InstText")),

             includeHTML("Download.html"),
             
             # Output
             h3(textOutput("InputText")),
             tableOutput("TabSNdata")
           )
  )

# Tab: Result ----
TabResult <- 
  tabPanel("Result", 
           # sidebarPanel - for Input
           sidebarPanel(
             # Title
             h3("Prediction"),
             
             # Input: Demand life
             numericInput("pred.S.from.N", 
                          label = "Demand life", 
                          value = "2e6"),
             
             # Input: Demand life
             numericInput("pred.N.from.S", 
                          label = "Demand load", 
                          value = ""),
             
             # Input: Interval (None / Pred / Conf /Both) [%]
             selectInput("int", label = "Interval", 
                         c("None", "Prediction", "Confidence", "Both"), 
                         selected ="Prediction"),
             
             # Input: Confidence level [%]
             numericInput("conf.level", 
                          label = "Confidence level [%]", 
                          value = 90),
             
             h3("Prior knowledge"),
             
             # Input: Prior knowledge Slope (low,high)
             fluidRow(
               column(6,
                      # Input: Slope (low)
                      numericInput("b.low", 
                                   label = "Slope (low)", 
                                   value = NA)
               ),
               column(6,
                      # Input: Slope (high)
                      numericInput("b.high", 
                                   label = "Slope (high)", 
                                   value = NA)
               )
             ),
             
             
             # Input: Prior knowledge CoV (low,high)
             fluidRow(
               column(6,
                      # Input: CoV (low)
                      numericInput("CoV.median", 
                                   label = "CoV (median)", 
                                   value = NA)
               ),
               column(6,
                      # Input: CoV (high)
                      numericInput("CoV.high", 
                                   label = "CoV (high)", 
                                   value = NA)
               )
             ), # END fluidRow
             
             
             # Submitt button: Update
             submitButton("Update"),
             
             hr(),
             
             h3("Plotting"),
             
             # Input: Show equation / Plot demand life
             checkboxInput("showEq", label = "Show Equation", value=TRUE),
             #             checkboxInput("plot.pred.S.from.N", label = "Plot demand life", value=TRUE),
             
             # Input: Prior knowledge CoV (low,high)
             # fluidRow(
             #   column(6,
             # Input: Unit
             textInput("unit",
                       label = "Unit",
                       value = "MPa"),
             #   ),
             #   column(6,
             # Input: Variable
             # textInput("loadVal", 
             #           label = "Variable", 
             #           value = DeltaS)
             #   )
             # ), # END fluidRow
             
             # Input: Title, Ylabel & Xlabel
             textInput("title", label = "Title", 
                       value = ""),  
             textInput("ylab", label = "Ylabel", 
                       value = paste(DeltaS, ", Load range", sep="")),   
             textInput("xlab", label = "Xlabel", 
                       value = "N, Number of cycles to failure"),
             
             # Input: x-axis
             fluidRow(
               column(6,
                      # Input: x-axis, min
                      numericInput("xlim.min", 
                                   label = "x-axis, min", 
                                   value = NA)
               ),
               column(6,
                      # Input: x-axis, max
                      numericInput("xlim.max", 
                                   label = "x-axis, max", 
                                   value = NA)
               )
             ), # END fluidRow
             
             # Input: y-axis
             fluidRow(
               column(6,
                      # Input: y-axis, min
                      numericInput("ylim.min", 
                                   label = "y-axis, min", 
                                   value = NA)
               ),
               column(6,
                      # Input: y-axis, max
                      numericInput("ylim.max", 
                                   label = "y-axis, max", 
                                   value = NA)
               )
             ), # END fluidRow
             
             # Submitt button: Update
             submitButton("Update")
           ),
           
           # mainPanel - for Output
           # Show a plot of the data and estimated endurance limit and print results
           mainPanel(
             # Title & Info
             h2("Result: Estimated Wöhler curve"),
             p(textOutput("InfoText1")),
             
             # Plot results
             plotOutput("SNplot"),
             
             # Present results in three paragraphs
             p(textOutput("ResultText1")),
             p(textOutput("ResultText2")),
             p(textOutput("ResultText3"))
           )
  )





