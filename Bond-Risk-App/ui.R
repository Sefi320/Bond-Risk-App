library(shiny)
library(bslib)
library(plotly)
library(DT)

default_date <- "2026-01-05"

bslib::page_navbar(
  title = "Risk Management",
  fillable = FALSE,
  theme = bslib::bs_theme(bootswatch = "lux"),
  navbar_options = bslib::navbar_options(theme = "dark"),
  
  bslib::nav_panel(title = "Historical Analysis",
                   bslib::card(
                     bslib::card_header("Historical FRED Treasury Bonds"),
                     shiny::selectInput("chart_choice", "Select Chart to Display:",
                                        choices = c(
                                          "Duration" = "duration_chart",
                                          "Convexity" = "convexity_chart",
                                          "Delta" = "delta_chart",
                                          "Volatility" = "volatility_chart"),
                                        selected = "duration_chart"),
                     plotly::plotlyOutput("historical_chart")
                   )),
  
  bslib::nav_panel(title = "Market Conditions",
                   bslib::card(
                     bslib::card_header("Yield Curve"),
                     shiny::dateInput("yield_date","Select Day",
                                      value = default_date),
                     shiny::uiOutput("date_msg"),
                     plotly::plotlyOutput("yield_curve")
                   ),
                   bslib::card(
                     bslib::card_header("Zero Curve (from DiscountCurve)"),
                     plotly::plotlyOutput("zero_curve_plot")
                   )),
  
  bslib::nav_panel(title = "Portfolio",
                   bslib::card(
                     bslib::card_header("Portfolio Settings"),
                     shiny::dateInput("portfolio_valuation_date", "Portfolio Valuation Date:", value = default_date,max = Sys.Date())),
                   bslib::layout_sidebar(
                     sidebar = bslib::sidebar(
                       title = "Add Bond",
                       shiny::dateInput("issue_date", "Issuance Date: ", value = default_date),
                       shiny::numericInput("rate_input", "Rate (%):", value = 5, min = 0, step = 0.25),
                       shiny::numericInput("maturity_input", "Maturity:", value = 10, min = 0.25, step = 0.25),
                       shiny::numericInput("quantity_input", "Quantity (#):", value = 1, min = 1, step = 1),
                       shiny::numericInput("price_input", "Face Value ($):", value = 100, min = 100, step = 100),
                       shiny::actionButton("add_bond", "Add to Portfolio", class = "btn-primary"),
                       shiny::actionButton("delete_bond", "Delete Selected Row", class = "btn-danger", icon = shiny::icon("trash-can", lib = "font-awesome")),
                       shiny::actionButton("calculate_value", "Calculate Portfolio Value", class = "btn-success", icon = shiny::icon("calculator", lib = "font-awesome"))
                       ),
                     bslib::card(
                       bslib::card_header("Current Portfolio"),
                       DT::DTOutput("portfolio_table")
                     )),
                   bslib::layout_columns(
                     bslib::card(
                       card_header("Allocation"),
                       card_body(plotlyOutput("port_pie"))
                     ), 
                     layout_columns(
                       bslib::card(
                         card_header("Portfolio Value"),
                         textOutput("portfolio_value")),
                       
                       bslib::card(
                         card_header("Delta"),
                         textOutput("port_duration")
                       ),
                       bslib::card(
                         card_header("DV01_Gamma"),
                         textOutput("port_convex")
                         ),
                       bslib::card(
                         card_header("DV01_Delta"),
                         textOutput("DV_01_delta")),
                       ))),
  
  bslib::nav_panel(title = "Scenario Analysis",
                   plotly::plotlyOutput("parallel_shift_plot"),
                   DT::dataTableOutput("parallel_shift_dt")
                   ))
