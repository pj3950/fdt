#---------------------------------
# Shiny App: Endurance Limit Estimation
#
# Pär Johannesson, 14-Aug-2020
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

source("app.tabs.R")
source("app.tab.about.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("Endurance Limit Estimation - RISE Fatigue Design Tool"),
  
  
  # Output: Tabset w/ plot, summary, and table ----
  tabsetPanel(#type = "tabs",
    
    # Tab: Input ----
    TabInput,
    
    # Tab: Result ----
    TabResult,
    
    # Tab: About ----
    fdt.tabAbout
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
      Fname <- "data/fatlimnytt.txt"

    # Read file to data.frame
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
    
    if(is.na(FL$ErrorText))
    {
      
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
    }
    else
      return(NULL)
    
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
    else {
      if(is.na(FL$ErrorText))
        return(paste0("The estimated endurance limit at ", input$RO, " cycles with ",  
                      input$conf.level, "% confidence interval is plotted together with the fatigue test data. ",
                      "The estimated parameters and prediction limits are presented below the graph."
        ))
      else
        return(FL$ErrorText)
    }
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
    
    if(is.na(FL$ErrorText))
    {
      
      # Plot data & results
      title <- input$title
      xlab <- input$xlab
      ylab <- input$ylab
      if(nchar(title)==0) {title <- paste0("Endurance limit with ", input$conf.level, "% confidence limits")}
      if(nchar(input$unit)>0) {ylab <- paste0(ylab, " [", input$unit, "]")}
      
      return(fatlim.plot(FL, title=title, xlab=xlab, ylab=ylab, ylim=NULL))
    }
    else # Error: Only plot data
    {
      # Plot data
      title <- input$title
      xlab <- input$xlab
      ylab <- input$ylab
      if(nchar(title)==0) {title <- paste0("Test data")}
      if(nchar(input$unit)>0) {ylab <- paste0(ylab, " [", input$unit, "]")}
      
      return(fatlim.plot(FL, title=title, xlab=xlab, ylab=ylab, ylim=NULL))
    }
    
    
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

