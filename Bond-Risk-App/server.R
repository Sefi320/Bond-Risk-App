library(shiny)
library(tidyverse)
library(bslib)
library(corrplot)
library(DT)
library(tibble)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  x <- reactive({
    req(input$yield_date)
    
    available_dates <- sort(unique(as.Date(cmt_clean$date))) # Finds available dates from df
    selected <- as.Date(input$yield_date) # User selected date
    
    closest_date <- available_dates[
      which.min(abs(available_dates - selected)) # snaps to closest date available if there is no data for the selected day (i.e weekends)
    ]
    
    y <- cmt_clean %>%
      filter(as.Date(date) == closest_date)
    
    list(
      data = y,
      selected = selected,
      closest_date = closest_date
    )
    
  })
  
  observeEvent(input$yield_date, {
    
    selected <- as.Date(input$yield_date)
    available_dates <- sort(unique(as.Date(cmt_clean$date)))
    closest_date <- available_dates[
      which.min(abs(available_dates - selected))
    ]
    
    if (closest_date != selected) {
      updateDateInput(session = session,"yield_date", value = closest_date)
    }
  })
  
  output$date_msg <- renderUI({
    result <- x()
    
    if (result$selected != result$closest_date) {
      showNotification(
        paste0("Selected Date Not Available."," Showing ",result$closest_date," instead."),
        duration = 7,
        type = "warning"
      )
    }
    
  })
  
  output$yield_curve <- renderPlotly({
    
    df <- x()
    df$data$symbol <- factor(df$data$symbol, levels = order)
    
    
    plotly::plot_ly(
      data = df$data,
      x = ~maturity,
      y = ~rate,
      text = ~df$symbol,
      hoverinfo = "text+y",
      type = "scatter",
      mode = "lines+markers"
    ) %>%
      plotly::layout(
        xaxis = list(
          title = "Maturity",
          tickmode = "array",
          tickvals = df$data$maturity,
          ticktext = df$data$symbol
        ),
        yaxis = list(title = "Yield")
      )
  })
  
  cor <- cmt_clean %>%
    dplyr::select(date,symbol,change_bps) %>%
    tidyr::pivot_wider(
      names_from = symbol,
      values_from = change_bps) %>%
    dplyr::select(-date) %>%
    stats::cor()
  
  output$cor_map <- renderPlot({
    corrplot::corrplot(cor,
                       type = "lower",
                       tl.col = "black",
                       tl.srt = 45,
                       method = "shade")
    
  })
  
  # -----
  # green portfolio section
  # -----
  
  
  portfolio_data <- reactiveVal(
    tibble::tibble(
      Bond = character(),
      Rate = numeric(),
      Maturity = numeric(),
      Quantity = numeric(),
      'Face Value' = numeric()
    )
  )
  
  # adds a row
  observeEvent(input$add_bond, {
    new_row <- tibble(
      Bond = input$bond_selector,
      Rate = input$rate_input,
      Maturity = input$maturity_input,
      Quantity = input$quantity_input,
      'Face Value' = input$face_input
    )
    portfolio_data(dplyr::bind_rows(portfolio_data(), new_row))
  })
  
  # deletes a row
  observeEvent(input$delete_bond, {
    req(input$portfolio_table_rows_selected)
    portfolio_data(portfolio_data()[-input$portfolio_table_rows_selected, ])
  })
  
  # edits a row
  observeEvent(input$portfolio_table_cell_edit, {
    info <- input$portfolio_table_cell_edit
    updated_data <- DT::editData(portfolio_data(), info, rownames = FALSE)
    portfolio_data(updated_data)
  })
  
  output$portfolio_table <- DT::renderDataTable({
    DT::datatable(
      portfolio_data(),
      rownames = FALSE,
      selection = "single",
      editable = list(
        target = 'cell',
        disable = list(columns = 0))
    )
  })
  
  # Zero rate
  curve_tbl <- reactive({
    df <- x()
    build_curve_tbl(df$data, df$closest_date, grid_times = seq(0.25, 30, by = 0.25), m = 2)
  })
  
  # Zero rate plot output
  output$zero_curve_plot <- renderPlotly({
    ct <- curve_tbl()
    
    plotly::plot_ly(
      data = ct,
      x = ~ maturity,
      y = ~ zero_rate,
      type = "scatter",
      mode = "lines"
    ) %>%
      plotly::layout(
        xaxis = list(title = "Maturity (years)"),
        yaxis = list(title = "Zero Rate"),
        title = paste0("Zero Curve (loglinear DF) — ", x()$closest_date)
      )
  })
  
  # Table output to see different maturities, DFs and Zero Rates
  output$zero_curve_table <- DT::renderDataTable({
    ct <- curve_tbl()
    
    DT::datatable(
      ct %>% transmute(
        maturity,
        zero_rate_pct = round(zero_rate, 4),
        discount_factor = round(discount_factor, 6)
      )
    )
  })
  
}