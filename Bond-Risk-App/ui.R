


library(shiny)
library(bslib)
library(plotly)


default_date <- "2026-01-05"

page_navbar(
  title = "Risk Management",
  theme = bs_theme(),
  nav_panel(title = "Market Conditions", p("Orange Section of Mental Model"),
            card(
              card_header("Yield Curve"),
              dateInput("yield_date","Select Day",
                        value = default_date),
              uiOutput("date_msg"),
              plotly::plotlyOutput("yield_curve")
            ),
            card(
              card_header("Correlation Matrix"),
              plotOutput("cor_map")
            )),
  nav_panel(title = "Portfolio", p("Green Section of Mental Model including Risk metrics")),
  nav_panel(title = "Scenario Analysis", p("Blue section of mental model.")),
  nav_spacer(),
  
  navbar_options = navbar_options(
    bg = "steelblue",
    inverse = T
  )
  )
