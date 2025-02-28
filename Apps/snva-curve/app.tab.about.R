# =============================================================
# Toolbox for RISE Fatigue Design Tool
#
# Pär Johannesson, 06-Nov-2023
#
# Content:
#   fdt.tabAbout    Tab About
# =============================================================


# =============================================================
# Tab: About ----
fdt.tabAbout <-
  tabPanel("About",
           
           # # mainPanel - for Output
           # Show About and licence
           mainPanel(
             
             h2("About"),
             
             # Version
             p(strong(includeText("About/APP.txt"))),
             p(strong(includeText("About/VERSION.txt"))),
             
             # RISE Fatigue Design Tool
             h3("RISE Fatigue Design Tool"),
             includeHTML("About/About.html"),
             
             # Licence
             h3("Licence"),
             
             pre(includeText("About/LICENCE.txt"))
             
           )
  )
# =============================================================

