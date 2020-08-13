# R-app: Load Analysis for Fatigue
#
# Pär Johannesson, 03-Sep-2018, 13-11-2018
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# =============================================================


library(shiny)

source("LoadAnalysisTools.r")

#---------------------------------

RFw<-function(TS,h,b)
{
  # Load history
  TP0 <- ts2tp(TS)                    # Turning Points
  RFC0 <- tp2rfc(TP0)                 # Rainflow cycles
  D0 <- sum(abs(RFC0[,1]-RFC0[,2])^b) # Pseudo damage
  N0 <- length(TP0)/2                 # Number of cycles
  
  #Rainflow filtered load history
  h.input <- h                                # Input threshold range
  if(h<0) {h <- -h/100*diff(range(TP0[,2]))}  # If negative (threshold range in %), then calculate threshold range
  h.rel <- 100*h/diff(range(TP0[,2]))         # Threshold range in % of total range
  #  TP <- ts2tp(TS,h)
  TP <- ts2tp(TP0,h)                # Turning Points
  RFC <- tp2rfc(TP)                 # Rainflow cycles
  D <- sum(abs(RFC[,1]-RFC[,2])^b)  # Pseudo damage
  N <- length(TP)/2                 # Number of cycles
  
  
  # Set output
  RF <- list("TP")
  RF$TP <- TP
  RF$TP0 <- TP0
  RF$RFC <- RFC
  RF$RFC0 <- RFC0
  RF$Drel <- D/D0
  RF$Nacc <- N0/N
  RF$N <- N
  RF$N0 <- N0
  RF$h <- h
  RF$h.input <- h.input
  RF$h.rel <- h.rel
  
  # Return output
  RF
}



#========================================================
# Define UI for application 
#========================================================

source("app.tabs.R")
source("app.tab.about.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("Load Analysis for Fatigue"),
  
  
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

# END: UI


# =============================================================
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create the object with no values
  # values <- reactiveValues()
  # values$loadExample <- FALSE
  
  # ------------
  # Global varables
  loadExample <- reactiveVal(0)
  
  # ------------
  # Functions
  # TS0 <- reactive({
  #   inFile <- input$file
  # 
  #   if (is.null(inFile))
  #     return(NULL)
  # 
  #   # Läs in datafilen till en dataframe
  #   TS0 <- read.table(inFile$datapath, header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
  # 
  #   TS0
  # })
  
  # ------------
  # Functions
  # TS0ex <- reactive({
  #   loadEx <- loadExample()
  # 
  #   if(loadEx == 1)
  #   {
  #     Fname <- "load_short.txt"
  #   }
  #   else
  #     return(NULL)
  # 
  #   # Läs in datafilen till en dataframe
  #   TS0 <- read.table(Fname, header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
  # 
  #   TS0
  # })
  # 
  # RFcalc <- reactive({
  #   print("RFcalc")
  #   
  #   TS0 <- TS0()
  #   TS0ex <- TS0ex()
  #   
  #   if (is.null(TS0) & is.null(TS0ex))
  #       return(NULL)
  #   
  #   TS <- as.matrix(TS0)
  #   
  #   # Beräkna Rainflow filter
  #   RF <- RFw(TS, h=input$h, b=input$b)
  #   RF$TS <- TS
  #   
  #   RF
  # })
  # 
  
  # ------------
  # Functions
  RFcalc_old <- reactive({
    inFile <- input$file
    
    print(input$file)
    print(input$file)
    
    if (is.null(inFile))
        return(NULL)

    # Läs in datafilen till en dataframe
    TS0 <- read.table(inFile$datapath, header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
    TS <- as.matrix(TS0)
    
    # Beräkna Rainflow filter
    RF <- RFw(TS, h=input$h, b=input$b)
    RF$TS <- TS
    
    RF
  })

#  RFcalc <- reactive({
  RFcalc <- reactive({
    
#    print(input$file)
    
    inFile <- input$file
    loadEx <- loadExample()
      

    if (!is.null(inFile))
      Fname <- inFile$datapath
    else
      Fname <- "data/load_short.txt"
    
    # {
    #   # if(values$loadExample)
    #   #   # if(!is.null(values))
    #   #   {
    #   #   Fname <- "load_short.txt"
    #   # }
    #   if(loadEx == 1)
    #   {
    #     Fname <- "load_short.txt"
    #   }
    #   else
    #     return(NULL)
    # }
    
    
#    Fname ="load_short.txt"
    
    # Läs in datafilen till en dataframe
    #TS0 <- read.table(inFile$datapath, header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
    TS0 <- read.table(Fname, header=FALSE, stringsAsFactors=FALSE, fill=TRUE)
    TS <- as.matrix(TS0)
    
    # Beräkna Rainflow filter
    RF <- RFw(TS, h=input$h, b=input$b)
    RF$TS <- TS
    
    RF
  })
  
  output$available <- reactive({
    RF <- RFcalc()

    if (is.null(RF)){
      return(FALSE) }
    else {
      return(TRUE)}
  })

  outputOptions(output, "available", suspendWhenHidden = FALSE)


  # ------------
  # Instructions: 
  output$InstText <- renderText({ 
    
    # Check if Estimate fatigue limit
    return(paste0("Here you can rainflow filter a load history, i.e. extract the turning points of a load and remove small cycles. ",
                  "First you have to upload a file on the left hand side. You have to specify a data file to be uploaded. ", 
                  "The load history is specified in a text file with two columns, where the first column contains time and the second load values. ", 
                  "When the file is uploaded the calculation will be performed and the results will be shown in tab Result. ", 
                  "Negative values of threshold range is interpreted as percentage of global load range, e.g. a value -10 gives a threshold range representing 10% of the max range of the signal.")
    )
  })
  
  # ------------
  # Data: 
  output$InputText <- renderText({ 
    
    if (!is.null(input$file))
      text <- "Input Load Signal"
    else
      text <- "Example Load Signal"
    
    # Check if Estimate fatigue limit
    return(text)
  })
  

#   observeEvent(input$LoadShortLS, {
# #    FnameEx <<- "load_short.txt"
#     loadExample(1)
#     
#     # values$loadExample <<- TRUE
#     # values$Fname <<- "load_short.txt"
#     # input$file <- NA
#     
# #    input$file <- data.frame(datapath="./load_short.txt")
#     # Fname<-"load_short.txt"
#     # dat0 <- read.table(Fname, sep="",dec=".",header=FALSE, skip=0, as.is=TRUE)
#     # names(dat0) <- c("t", "L")
#   })
  
  output$InputPlot <- renderPlot({
    
    # Debugging
    #TS0 <- read.table("load_short.txt",header=FALSE,stringsAsFactors=FALSE,fill=TRUE)
    #TS <- as.matrix(TS0)
    #RF <- RFw(TS, h=1, b=5)
    
    
    # Beräkna Rainflow filter
    RF <- RFcalc()
    
    if (is.null(RF))
      return(NULL)
    
    # Resultaten plottas
    
    # Plot load signal
    plot(RF$TS[,1],RF$TS[,2], type="l", col="blue", 
         main="Load history", xlab=input$xlab, ylab=input$ylab)
    mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7,col="blue",adj=1)
  })
  
  
  output$RFplot <- renderPlot({
    
    # Debugging
    #TS0 <- read.table("load_short.txt",header=FALSE,stringsAsFactors=FALSE,fill=TRUE)
    #TS <- as.matrix(TS0)
    #RF <- RFw(TS, h=1, b=5)
    
    
    # Beräkna Rainflow filter
    RF <- RFcalc()
    
    if (is.null(RF))
      return(NULL)
    
    # Resultaten plottas
    
    # Plot load signal
    plot(RF$TS[,1],RF$TS[,2], type="l", col="blue", 
         main="Load history", xlab=input$xlab, ylab=input$ylab)
    points(RF$TP0[,1],RF$TP0[,2], type="p", pch=20, col="black")
    lines(RF$TP[,1],RF$TP[,2],type="b",pch=10,col="red")
    mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7,col="blue",adj=1)
  })
  
  output$RFplot2 <- renderPlot({
    
    # Debugging
    #TS0 <- read.table("load_short.txt",header=FALSE,stringsAsFactors=FALSE,fill=TRUE)
    #TS <- as.matrix(TS0)
    #RF <- RFw(TS, h=1, b=5)
    
    
    # Beräkna Rainflow filter
    RF <- RFcalc()
    
    if (is.null(RF))
      return(NULL)
    
    # Resultaten plottas
    
    split.screen(c(1,2))    # split bottom half in two
    
    # Plot Level crossings
    screen(1)
    rfc2lc(RF$RFC0, title="Level crossings")
    rfc2lc(RF$RFC, new=TRUE)
    mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7,col="blue",adj=1)

    # Plot Load spectrum
    screen(2)
    rfc2ls(RF$RFC0, title="Load spectrum") 
    rfc2ls(RF$RFC, new=TRUE, N=RF$N0-RF$N)
    mtext("RISE Fatigue Design Tool",3,0,font=21,cex=0.7,col="blue",adj=1)
  })
  
  
  output$ResultHeader <- renderText({ 
    paste("Rainflow Filtered Turning Points")
  })
  
  
  output$ResultHeader2 <- renderText({ 
    paste("Level Crossings and Load Spectrum")
  })
  
  
  output$ResultHeader3 <- renderText({ 
    paste("Results from the Analysis")
  })
  
  
  # Result Text 1: 
  output$ResultText1 <- renderText({ 
    # Beräkna Rainflow filter
    RF <- RFcalc()
    
    if (is.null(RF))
      return(NULL)
    
    # Resultaten skrivs ut
    paste("The load history contains ",RF$N0," turning points. ",
          "By applying a rainflow filter with a threshold range of ", signif(RF$h,3),
          " (", signif(RF$h.rel,3), "% of max range), the number of turning points is reduced to ",RF$N,". ",
          "This means an acceleration by a factor of ",signif(RF$Nacc,3),
          ", but in this case keeping ",round(100*RF$Drel, digits = 2),"% of the original damage, ",
          "based on Rainflow cycle counting, the Palmgren-Miner rule and a damage exponent of ",input$b,".",
          sep="")
  })
  
  
  output$ResultTableHeader <- renderText({ 
    if(!input$printTable)
      return(NULL)
    
    paste("Table of rainflow filtered turnign points")
  })
  
  
  output$ResultTable <- renderTable({ 
    
    if(!input$printTable)
      return(NULL)
    
    # Beräkna Rainflow filter
    RF <- RFcalc()
    
    if (is.null(RF))
      return(NULL)
    
    # Resultaten skrivs ut
    colnames(RF$TP) <- c("Time","Value")
    return(RF$TP)
  })
  
  
  output$downloadTP <- downloadHandler(
    
    # Download TP
    filename = function() {
      paste('TP-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      # Beräkna Rainflow filter
      RF <- RFcalc()
      
      # if (is.null(RF))
      #   return(NULL)
      data <- data.frame(time=RF$TP[,1], value=RF$TP[,2])
      write.table(data.frame(Time=RF$TP[,1], Value=RF$TP[,2]), file, sep=";", row.names = FALSE)
    }
  )
  
}
# END: Server

# Run the application 
shinyApp(ui = ui, server = server)











