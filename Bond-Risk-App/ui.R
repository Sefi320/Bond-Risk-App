library(shiny)
library(bslib)
library(plotly)
library(DT)


default_date <- "2026-01-05"

page_navbar(
  title = "Risk Management",
  fillable = FALSE,
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
            ),
            card(
              card_header("Zero Curve (from DiscountCurve)"),
              plotly::plotlyOutput("zero_curve_plot"),
              DT::DTOutput("zero_curve_table")
              )),
  
  
  
  nav_panel(title = "Portfolio", p("Green Section of Mental Model including Risk metrics"),
            page_sidebar(
              sidebar = sidebar(
                title = "Add Bond",
                selectInput("bond_selector", "Select Bond:", choices = order),
                numericInput("maturity_input", "Maturity:", value = 10, min = 0.25, step = 0.25),
                numericInput("rate_input", "Rate (%):", value = 5, min = 0, step = 0.25),
                numericInput("price_input", "Price ($):", value = 100, min = 0, step = 0.5),
                actionButton("add_bond", "Add to Portfolio")
              ),
              card(
                card_header("Current Portfolio"),
                DT::DTOutput("portfolio_table"),
                actionButton("delete_bond", "Delete Selected Row", class = "btn-danger", icon = icon("trash-can", lib = "font-awesome"))
              ))),
  
  
  
  nav_panel(title = "Scenario Analysis", p("Blue section of mental model.")),
  nav_spacer(),
  
  navbar_options = navbar_options(
    bg = "steelblue",
    inverse = T
  )
  )
