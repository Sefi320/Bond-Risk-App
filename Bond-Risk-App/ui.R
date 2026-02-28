library(shiny)
library(bslib)
library(plotly)
library(DT)

default_date <- "2026-01-05"

bslib::page_navbar(
  title = "Risk Management",
  fillable = FALSE,
  theme = bslib::bs_theme(bootswatch = "lux"),
  
  bslib::nav_panel(title = "Market Conditions", p("Orange Section of Mental Model"),
            bslib::card(
              bslib::card_header("Yield Curve"),
              shiny::dateInput("yield_date","Select Day",
                        value = default_date),
              shiny::uiOutput("date_msg"),
              plotly::plotlyOutput("yield_curve")
            ),
            bslib::card(
              bslib::card_header("Correlation Matrix"),
              shiny::plotOutput("cor_map")
            ),
            bslib::card(
              bslib::card_header("Zero Curve (from DiscountCurve)"),
              plotly::plotlyOutput("zero_curve_plot"),
              DT::DTOutput("zero_curve_table")
              )),
  
  bslib::nav_panel(title = "Portfolio", p("Green Section of Mental Model including Risk metrics"),
            bslib::layout_sidebar(
              sidebar = bslib::sidebar(
                title = "Add Bond",
                shiny::selectInput("bond_selector", "Select Bond:", choices = order),
                shiny::numericInput("rate_input", "Rate (%):", value = 5, min = 0, step = 0.25),
                shiny::numericInput("maturity_input", "Maturity:", value = 10, min = 0.25, step = 0.25),
                shiny::numericInput("quantity_input", "Quantity ($):", value = 1000, min = 1000, step = 1000),
                shiny::numericInput("price_input", "Price ($):", value = 100, min = 0, step = 0.5),
                shiny::actionButton("add_bond", "Add to Portfolio")
              ),
              bslib::card(
                bslib::card_header("Current Portfolio"),
                DT::DTOutput("portfolio_table"),
                shiny::actionButton("delete_bond", "Delete Selected Row", class = "btn-danger", icon = shiny::icon("trash-can", lib = "font-awesome"))
              ))),
  
  bslib::nav_panel(title = "Scenario Analysis", p("Blue section of mental model."))
  )