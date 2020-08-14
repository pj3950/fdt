#---------------------------------
# Shiny App: Wöhler Curve Estimation
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

source("SNwPJ_v2.4.R")
#source("TbxFatigueDesignTool.R")


#========================================================
# Define UI for application 
#========================================================

source("app.tabs.R")
source("app.tab.about.R")

ui <- fluidPage(
  
  # Application title
  titlePanel("Wöhler Curve Estimation - RISE Fatigue Design Tool"),
  
  
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
  SNcalc0 <- function(dat) {
    
    # Plot results
    title <- input$title
    xlab <- input$xlab
    ylab <- input$ylab
#    if(input$plot.Npred) Npred<-input$Npred else Npred=0
    if(!is.nan(input$Npred)) Npred<-input$Npred else Npred=NA
#    Npred <- input$Npred
    if(nchar(title)==0) {
      title <- 
        switch(input$int,
               "None" = paste0("Wöhler curve"),
               "Prediction" = paste0("Wöhler curve with ", input$conf.level, "% prediction limits"),
               "Confidence" = paste0("Wöhler curve with ", input$conf.level, "% confidence limits"),
               "Both" = paste0("Wöhler curve with ", input$conf.level, "% confidence and prediction limits")
        )
    }
    Nrange <- c(input$xlim.min, input$xlim.max)
    Srange <- c(input$ylim.min, input$ylim.max)
    
    if(nchar(input$unit)>0) {ylab <- paste0(ylab, " [", input$unit, "]")}
    
    SN <- SNw(dat$S, dat$N, dat$Fail, 
              conf.level=input$conf.level/100, Npred=Npred, #Nstrength=input$N.pred, 
              formel=input$showEq, int=input$int,
              beta_low=input$b.low, beta_high=input$b.high, S_prior=input$CoV.median, S_prior_high=input$CoV.high,
              title=title, xlab=xlab, ylab=ylab, Nrange=Nrange, Srange=Srange)
    #    fatlim.plot(SN, title=title, xlab=xlab, ylab=ylab, ylim=NULL)
    
    SN$dat <- dat
    
    SN
  }
  
  
  # ------------
  # Reactive function: Estimate Endurance limit when data has been loaded
  SNcalc <- reactive({
    inFile <- input$file
    
    # if (is.null(inFile))
    #   return(NULL)
    if (!is.null(inFile))
      Fname <- inFile$datapath
    else
      Fname <- "data/laser_cbj.txt"
    
    # Läs in datafilen till en dataframe
    dat <- read.table(Fname, sep="", dec=".", header=FALSE, skip=0, as.is=TRUE, fill=TRUE)
    #Kolla om vi har en tredje kolumn, om inte skapa en sådan.
    if (ncol(dat)<3)  dat$fail="F"
    
    names(dat) <- c("S, Load range", "N, Number of cycles", "Failure (F/RO)")
    #    names(dat) <- c("Specimen number", "Load level", "Failure")
    
    # Estimate Endurance limit
    SN <- SNcalc0(dat)
    # SNw(dat$S,dat$N,dat$fail, beta_low=input$b.low, beta_high=input$b.high, #beta_low=beta_prior[1], beta_high=beta_prior[2],
    #           conf.level=input$conf.level/100, Nstrength=input$N.pred, Npred=input$N.pred,
    #            title="SN-curve", ylabel="Load", xlabel="N, number of cycles to failure")
    
    #    FL <- fatlim.est(dat, conf.level=input$conf.level/100, plot=0)
    
    SN
  })
  
  
  # ------------
  # Function: Compile results in three strings (mean, std & prediction)
  #  res <- list(coef=coef, ci=ci, info = c(conf.level=conf.level, df=df, q=q), M=M, coef0=coef0, ci0=ci0)
  # result<-list(c("Se"=result.Se, "k"=result.k, "s"=result.COV, "kint"=result.k.int, "result.k.pm"=result.k.pm, 
  #                "strength.ci"=result.Strength.ci, "strength.int"=result.Strength.int, "tau"=result.Strength.s,
  #                "loglimx"=result.limxtemp,"loglimy"=result.limytemp,
  #                "COV"=result.COV,
  #                "DNVstrength"=signif(StrengthDNV,3)))
  
  resultTxt <- function()
  {
    SN0 <- SNcalc()
    
    if (is.null(SN0))
      return(NULL)
    
    #    print(SN0)
    
    
    res0<- SN0
    SN <- list(coef=NA, ci=NA)
    SN$coef <- data.frame(b=NA, FAT=NA, s=NA)
    SN$coef$b <- res0$k
    SN$coef$FAT <- res0$Se
    SN$coef$s <- res0$s
    
    parnames <- c("b","FAT","s")
    ci <- matrix(NA,3,2, dimnames=list(parnames,c("low","high")))
    ci[1,1:2] <- res0$kint
    #    ci[1,2] <- res0[5,1]
    ci[2,1:2] <- res0$strength.ci
    #    ci[2,2] <- res0[8,1]
    SN$ci <- ci
    
    SN$strength.int1 <- res0$strength.int[1]
    SN$tau <- res0$tau
    
    #    print(SN)
    
    
    unit <- input$unit
    if(nchar(input$unit)>0) {unit <- paste0(" ", input$unit)}
    
    out1 <- paste0("The fatigue strength at two million cycles is is estimated Se = ", signif(SN$coef$FAT,3), unit, " and ",
                   "the Wöhler slope is estimated to k = ", signif(SN$coef$b,3), ". ",
                   "A 95% confidence intervals for the fatigue strength is [",
                   #                   "A ", input$conf.level, "% confidence intervals for the fatigue strength is [",
                   signif(SN$ci[2,1],3)," ; ", signif(SN$ci[2,2],3), "] ", unit, ", and for the slope [",
                   signif(SN$ci[1,1],3)," ; ", signif(SN$ci[1,2],3), "]."
    )
    
    out2 <- paste0("The coefficient of variation (CoV) for fatigue life is estimated to s = ", 
                   signif(SN$coef$s,2), ". "
                   # "A ", input$conf.level, "% confidence interval for the standard deviation is [", 
                   # signif(SN$sigmalim[1],3)," ; ", signif(SN$sigmalim[2],3), "]", unit, "."
    )
    
    out3 <- paste0("The lower ", (100-input$conf.level)/2, "% prediction limit for fatigue strength is ", 
                   signif(SN$strength.int1,3), unit, ". ",
                   "The prediction coefficient of variation for the load/strength is ", signif(SN$tau,3), ".")
    
    list(out1=out1, out2=out2, out3=out3)
    
  }
  
  
  # ------------
  # TAB: Input
  # ------------
  
  # ------------
  # Instructions: 
  output$InstText <- renderText({ 
    # Check if Estimate fatigue limit
    return(paste0("Here you can estimate a Wöhler curve with prediction intervals. First you have to upload a file on the left hand side. ",
                  "The data file should have three columns and the number of rows equal to the number of specimens. 
                    Each row contains the load level in the first column, the number of cycles in the second and the result failure/run-out, represented by 'F'/'RO', in the last column."
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
  # Table: SN data
  output$TabSNdata <- renderTable({ 
    # Check if Estimate fatigue limit
    SN <- SNcalc()
    
    if (is.null(SN))
      return(NULL)
    
    return(SN$dat)
  })
  
  
  # ------------
  # TAB: Results
  # ------------
  
  # ------------
  # Info Text 1: 
  output$InfoText1 <- renderText({ 
    # Check if Estimate fatigue limit
    SN <- SNcalc()
    
    if (is.null(SN))
      return(paste0("Here you can estimate a Wöhler curve with prediction intervals. First you need to upload a file in the tab input data "))
    else
      return(paste0(
        "The fatigue test results are plotted in the log-log diagram as shown below. ",
        "The classical Wöhler curve or SN-curve is a linear approximation in this log-log diagram of the constant amplitude fatigue life as a function of the load level. ",
        "The straight line in log-log scale can be expressed by the Basquin equation. ",
        "The estimated parameters and prediction limits are presented below the graph."
      ))
  })
  
  
  # ------------
  # Info Text 2: 
  output$InfoText2 <- renderText({ 
    # Check if Estimate fatigue limit
    SN <- SNcalc()
    
    if (is.null(SN))
      return("You have to specify a data file to be uploaded. The data file should have three columns and the number of rows equal to the number of specimens. Each row contains the run order in the first column, the recorded load amplitude in the second and the result failure/no failure, represented by '1'/'0', in the last column.")
    else
      return(NULL)
  })
  
  # ------------
  # Plot results
  output$SNplot <- renderPlot({
    # Estimate fatigue limit
    SN <- SNcalc()
    
    if (is.null(SN))
      return(NULL)
    
    # Plot results
    dat <- SN$dat
    
    SN <- SNcalc0(dat)
    
    # title <- input$title
    # xlab <- input$xlab
    # ylab <- input$ylab
    # if(nchar(title)==0) {title <- paste0("Wöhler curve with ", input$conf.level, "% prediction limits")}
    # if(nchar(input$unit)>0) {ylab <- paste0(ylab, " [", input$unit, "]")}
    # 
    # SNw(dat$S, dat$N, dat$fail, beta_low=input$b.low, beta_high=input$b.high,
    #     conf.level=input$conf.level/100, Nstrength=input$N.pred, Npred=input$N.pred,
    #     title=title, xlab=xlab, ylab=ylab)
    #    fatlim.plot(SN, title=title, xlab=xlab, ylab=ylab, ylim=NULL)
    
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
  
  # ------------
  # About text: Ver 
  output$AboutText1 <- renderText({ 
    "RISE Fatigue Design Tool has been developed at RISE Research Institutes of Sweden, department of Mechanics Research. It is a research platform implementing fatigue and load analysis tools to be used for fatigue design and testing."
    
  })
  
  # ------------
  # About text 2: (prediction)
  output$AboutText2 <- renderText({ 
    "The calculations are powered by R (a language and environment for statistical computing and graphics). The R-code for the fatigue calculations has been developed by Thomas Svensson and Pär Johannesson."
    
  })
  
  # ------------
  # About text: Licence
  output$AboutTextLicence <- renderText({ 
    "MIT Licence"
  })
  
}



#========================================================
# Run the application 
#========================================================

shinyApp(ui = ui, server = server)

