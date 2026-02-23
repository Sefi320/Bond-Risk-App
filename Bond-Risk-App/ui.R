


library(shiny)
library(bslib)

page_navbar(
  title = "Risk Management",
  theme = bs_theme(),
  nav_panel(title = "Market Conditions", p("Orange Section of Mental Model")),
  nav_panel(title = "Portfolio", p("Green Section of Mental Model including Risk metrics")),
  nav_panel(title = "Scenario Analysis", p("Blue section of mental model.")),
  nav_spacer(),
  
  navbar_options = navbar_options(
    bg = "steelblue",
    inverse = T
  )
  )
