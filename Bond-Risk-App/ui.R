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
                   
                   # bslib::layout_sidebar(
                   #   
                   #   sidebar = bslib::sidebar(
                   #     title = "Scenario Settings",
                   #     shiny::selectInput("scenario",label = "Scenario Type:",choices = c(
                   #       "Parallel Shift" = "parallel",
                   #       "Curve Steepening" = "steepening",
                   #       "Curve Flattening" = "flattening",
                   #       "Bear Flattening" = "bear_flat",
                   #       "Bull Steepening" = "bull_steep"
                   #     )),
                   #     
                   #     shiny::conditionalPanel(
                   #       condition = "input.scenario == 'parallel'",
                   #       shiny::numericInput("parallel_shift",
                   #                           label = "Parallel Shift (bps):",
                   #                           min = -300,
                   #                           max = 300,
                   #                           value = 100,
                   #                           step = 5)),
                   #     shiny::conditionalPanel(
                   #       condition = "input.scenario == 'steepening'",
                   #       shiny::numericInput(
                   #         "short_end_shift",
                   #         "Short End Shift (bps):",
                   #         min = -300,
                   #         max = 300,
                   #         value = 0,
                   #         step = 5
                   #       ),
                   #       shiny::numericInput(
                   #         "long_end_shift",
                   #         "Long End Shift (bps):",
                   #         min = -300,
                   #         max = 300,
                   #         value = 50,
                   #         step = 5
                   #       )
                   #     ),
                   #     
                   #     shiny::conditionalPanel(
                   #       condition = "input.scenario == 'flattening'",
                   #       shiny::numericInput(
                   #         "short_end_shift",
                   #         "Short End Shift (bps):",
                   #         min = -300,
                   #         max = 300,
                   #         value = 0,
                   #         step = 5
                   #       ),
                   #       shiny::numericInput(
                   #         "long_end_shift",
                   #         "Long End Shift (bps):",
                   #         min = -300,
                   #         max = 300,
                   #         value = 50,
                   #         step = 5
                   #       )
                   #     ),
                   #     
                   #     shiny::conditionalPanel(
                   #       condition = "input.scenario == 'bear_flat'",
                   #       shiny::numericInput(
                   #         "bear_flat_short",
                   #         "Short End Shift (bps):",
                   #         min = 0,
                   #         max = 300,
                   #         value = 100,
                   #         step = 5
                   #       ),
                   #       shiny::numericInput(
                   #         "bear_flat_long",
                   #         "Long End Shift (bps):",
                   #         min = 0,
                   #         max = 300,
                   #         value = 50,
                   #         step = 5
                   #       )
                   #     ),
                   #     
                   #     shiny::conditionalPanel(
                   #       condition = "input.scenario == 'bull_steep'",
                   #       shiny::numericInput(
                   #         "bull_steep_short",
                   #         "Short End Shift (bps):",
                   #         min = -300,
                   #         max = 0,
                   #         value = -50,
                   #         step = 5
                   #       ),
                   #       shiny::numericInput(
                   #         "bull_steep_long",
                   #         "Long End Shift (bps):",
                   #         min = -300,
                   #         max = 0,
                   #         value = 0,
                   #         step = 5
                   #       )
                   #     ),
                   #     
                   #     shiny::actionButton(
                   #       "run_scenario",
                   #       "Run Scenario",
                   #       class = "btn-primary",
                   #       icon = icon("play")
                   #     ))),
                   
                   # bslib::card(
                   #   card_header("Portfolio Parallel Shift"),
                   #   
                   #   plotly::plotlyOutput("parallel_shift_plot"),
                   #   DT::dataTableOutput("parallel_shift_dt")
                   # )
                   plotly::plotlyOutput("parallel_shift_plot"),
                   DT::dataTableOutput("parallel_shift_dt")
                   ))
