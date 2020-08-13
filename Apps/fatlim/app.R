#---------------------------------
# App: Endurance Limit Estimation
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#---------------------------------

library(shiny)

source("toolboxFatLim.R")


#========================================================
# Define UI for application 
#========================================================

#ui <- navbarPage(title="Endurance Limit Estimation",
ui <- fluidPage(
  
  # Application title
  titlePanel("Endurance Limit Estimation"),
  

  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(#type = "tabs",
    
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
  )
)



#========================================================
# Define server logic required to estimate endurance limit and plot result
#========================================================

server <- function(input, output) {
  
  # ------------
  # Reactive function: Estimate Endurance limit when data has been loaded
  FLcalc <- reactive({
    inFile <- input$file
    
    # if (is.null(inFile))
    #   return(NULL)
    if (!is.null(inFile))
      Fname <- inFile$datapath
    else
      Fname <- "fatlimnytt.txt"

    # Läs in datafilen till en dataframe
    dat <- read.table(Fname, sep="", dec=".", header=FALSE, skip=0, as.is=TRUE)
    names(dat) <- c("Specimen number", "Load level", "Failure")
    
    # Estimate Endurance limit
    FL <- fatlim.est(dat, conf.level=input$conf.level/100, plot=0)
    
    FL$dat <- dat
    
    FL
  })
  
  # ------------
  # Function: Compile results in three strings (mean, std & prediction)
  resultTxt <- function()
  {
    FL <- FLcalc()
    
    if (is.null(FL))
      return(NULL)
    
    unit <- input$unit
    if(nchar(input$unit)>0) {unit <- paste0(" ", input$unit)}
    
    out1 <- paste0("The mean of the endurance limit at ", input$RO, " cycles is estimated to ", signif(FL$my,3), unit, ". ",
                   "A ", input$conf.level, "% confidence interval for the endurance limit is [", 
                   signif(FL$mylim[1],3)," ; ", signif(FL$mylim[2],3), "]", unit, ".")
    
    out2 <- paste0("The standard deviation of the endurance limit is estimated to s = ", 
                   signif(FL$sigma,3), unit, ". ",
                   "A ", input$conf.level, "% confidence interval for the standard deviation is [", 
                   signif(FL$sigmalim[1],3)," ; ", signif(FL$sigmalim[2],3), "]", unit, ".")
    
    out3 <- paste0("The lower ", (100-input$conf.level)/2, "% prediction limit is ", 
                   signif(FL$predlow,3), unit, ". ",
                   "The prediction coefficient of variation is ", signif(FL$predstdmy,3), ".")
    
    list(out1=out1, out2=out2, out3=out3)
    
  }
  
  
  # ------------
  # TAB: Input
  # ------------
  
  # ------------
  # Instructions: 
  output$InstText <- renderText({ 
    # Check if Estimate fatigue limit
    return(paste0("Here you can calculate the endurance limit. First you have to upload a file on the left hand side. ",
                  "The data file should have three columns and the number of rows equal to the number of specimens. 
                    Each row contains the run order in the first column, the recorded load level in the second and the result failure/no failure, represented by '1'/'0', in the last column."
    ))
  })
  
  
  # ------------
  # Data: 
  output$InputText <- renderText({ 
    
    if (!is.null(input$file))
      text <- "Input Data"
    else
      text <- "Example Data"
    
    # Check if Estimate fatigue limit
    return(text)
  })
  
  
  # ------------
  # Table: FL data
  output$TabFLdata <- renderTable({ 
    # Check if Estimate fatigue limit
    FL <- FLcalc()
    
    if (is.null(FL))
      return(NULL)
    
    return(FL$dat)
  })
  
  
  # ------------
  # TAB: Results
  # ------------
  
  # ------------
  # Info Text 1: 
  output$InfoText1 <- renderText({ 
    # Check if Estimate fatigue limit
    FL <- FLcalc()
    
    if (is.null(FL))
      return(paste0("Here results for the estimated endurance limit are presented. First you need to upload a file in the tab input data "))
    else
      return(paste0("The estimated endurance limit at ", input$RO, " cycles with ",  
                    input$conf.level, "% confidence interval is plotted together with the fatigue test data. ",
                    "The estimated parameters and prediction limits are presented below the graph."
      ))
  })
  
  # ------------
  # Info Text 2: 
  output$InfoText2 <- renderText({ 
    # Check if Estimate fatigue limit
    FL <- FLcalc()
    
    if (is.null(FL))
      return("You have to specify a data file to be uploaded. The data file should have three columns and the number of rows equal to the number of specimens. Each row contains the run order in the first column, the recorded load amplitude in the second and the result failure/no failure, represented by '1'/'0', in the last column.")
    else
      return(NULL)
  })
  
  # ------------
  # Plot results
  output$FLplot <- renderPlot({
    # Estimate fatigue limit
    FL <- FLcalc()
    
    if (is.null(FL))
      return(NULL)
    
    # Plot results
    title <- input$title
    xlab <- input$xlab
    ylab <- input$ylab
    if(nchar(title)==0) {title <- paste0("Endurance limit with ", input$conf.level, "% confidence limits")}
    if(nchar(input$unit)>0) {ylab <- paste0(ylab, " [", input$unit, "]")}
    
    fatlim.plot(FL, title=title, xlab=xlab, ylab=ylab, ylim=NULL)
    
  })
  
  # ------------
  # Result output 1: (mean)
  output$ResultText1 <- renderText({ 
    txt <- resultTxt()
    
    if (is.null(txt))
      return(NULL)
    else
      return(txt$out1)
  })
  
  # ------------
  # Result output 2: (std)
  output$ResultText2 <- renderText({ 
    txt <- resultTxt()
    
    if (is.null(txt))
      return(NULL)
    else
      return(txt$out2)
  })
  
  # ------------
  # Result output 3: (prediction)
  output$ResultText3 <- renderText({ 
    txt <- resultTxt()
    
    if (is.null(txt))
      return(NULL)
    else
      return(txt$out3)
  })
  
}


#========================================================
# Run the application 
#========================================================

shinyApp(ui = ui, server = server)
