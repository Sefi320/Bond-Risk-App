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
  
  # -----
  # green portfolio section
  # -----
  
  
  portfolio_data <- reactiveVal(
    tibble::tibble(
      Date = as.Date(character()),
      Rate = numeric(),
      Maturity = numeric(),
      Quantity = numeric(),
      Price = numeric()
    )
  )
  
  # adds a row
  observeEvent(input$add_bond, {
    
    req(input$issue_date <= Sys.Date())
    
    output$issue_date_msg <- renderUI({
      
      
      if (input$issue_date >= Sys.Date()) {
        showNotification(
          paste0("Issue date can not be in the future."),
          duration = 7,
          type = "warning"
        )
      }
      
    })
    
    new_row <- tibble(
      Date = input$issue_date,
      Quantity = input$quantity_input,
      Maturity = input$maturity_input,
      Rate = input$rate_input,
      Price = input$price_input
    )
    portfolio_data(dplyr::bind_rows(portfolio_data(), new_row))
  })
  
  output$issue_date_msg <- renderUI({
    
    
    if (input$issue_date >= Sys.Date()) {
      showNotification(
        paste0("Issue date can not be in the future."),
        duration = 7,
        type = "warning"
      )
    }
    
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
  
  
  
  
  # Calculating Portfolio Value from user inputs
  
  portfolio_value <- reactive({
    
    d <- portfolio_data()
    req(nrow(d)>0)
    
    val_date <- input$portfolio_valuation_date
    req(!is.null(val_date))
    
    val_curve_data <- cmt_clean %>% 
      filter(as.Date(date) == as.Date(val_date))
    
    if (nrow(val_curve_data) == 0) {
      available_dates <- sort(unique(as.Date(cmt_clean$date)))
      closest <- available_dates[which.min(abs(available_dates - as.Date(val_date)))]
      val_curve_data <- cmt_clean %>% filter(as.Date(date) == closest)
      val_date <- closest
    }
    
    ct <- build_curve_tbl(val_curve_data, val_date)
    
    total_value <- 0
    
    
    for (bond in 1:nrow(d)) {
      
      
      T2M <- (d$Date[bond] + years(d$Maturity[bond]) - val_date) / 365
      
      if (T2M <= 0) next # If bond already matured, then it skips it
      
      cfs <- tibble::tibble(
        maturity = seq_back(T2M = T2M,step = 0.5)) %>% 
        dplyr::arrange(maturity) %>% 
        dplyr::mutate(
          CF = dplyr::case_when(
            maturity == 0 ~ 0,
            dplyr::row_number() == dplyr::n() ~ d$Price[bond] + (d$Price[bond] * (d$Rate[bond] / 2 /100)),
            TRUE ~ d$Price[bond] * d$Rate[bond] / 2/100)) %>%
        dplyr::mutate(maturity = round(maturity,2)) %>% 
        dplyr::left_join(.,ct, by = "maturity") %>% 
        dplyr::mutate(
          pv = discount_factor * CF)
      
      bond_value <- sum(cfs$pv, na.rm = TRUE) * (d$Quantity[bond])
      
      total_value <- total_value + bond_value
      
      
    }    
    
    total_value 
    
  })
  
  output$portfolio_value <- renderText({
    val <- portfolio_value()
    paste0("$", formatC(val, format = "f", digits = 2, big.mark = ","))
    
  })
  
}