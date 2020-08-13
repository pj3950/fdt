#' Tab "About" for Fatigue Shiny Apps
#'
#' @export

# =============================================================
# Toolbox for RISE Fatigue Design Tool
#
# Pär Johannesson, 30-Mar-2020
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
             p(strong(includeText("VERSION.txt"))),
             
             # RISE Fatigue Design Tool
             h3("RISE Fatigue Design Tool"),
             includeHTML("About.html"),
             
             # Licence
             h3("Licence"),
             
             pre(includeText("LICENCE.txt"))
             
           )
  )
# =============================================================

