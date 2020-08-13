# Tab: Input ----
TabTest <- tabPanel("Test",
         
         # # mainPanel - for Output
         # Show About and licence
         mainPanel(
           
           h2("Testing"),
           
           # Version
           p(strong(includeText("VERSION.txt"))),
           
           # RISE Fatigue Design Tool
           h3("RISE Fatigue Design Tool"),
           includeHTML("About.html"),
           
           # Licence
           h3("Licence"),
           
           pre(includeText("LICENCE.txt"))
           
         )
)

Tab2 <- tabPanel("Second Tab",
                 textOutput('mychoice'))