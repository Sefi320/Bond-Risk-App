library(shiny)
library(tidyverse)
library(bslib)
library(corrplot)
library(DT)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  # Historical Charts
  chart_duration <- reactive({
    plotly::plot_ly(
      data = chart_data,
      x = ~date,
      y = ~duration,
      color = ~symbol,
      type = "scatter",
      mode = "lines"
    ) %>% 
      plotly::layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Duration")
      )
  })
  
  chart_convexity <- reactive({
    plotly::plot_ly(
      data = chart_data,
      x = ~date,
      y = ~convexity,
      color = ~symbol,
      type = "scatter",
      mode = "lines"
    ) %>% 
      plotly::layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Convexity")
      )
  })
  
  chart_delta <- reactive({
    plotly::plot_ly(
      data = chart_data,
      x = ~date,
      y = ~delta,
      color = ~symbol,
      type = "scatter",
      mode = "lines"
    ) %>% 
      plotly::layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Delta")
      )
  })
  
  chart_volatility <- reactive({
    plotly::plot_ly(
      data = volatility_data,
      x = ~date,
      y = ~sd30,
      color = ~symbol,
      type = "scatter",
      mode = "lines"
    ) %>% 
      plotly::layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "30-Day Rolling Volatility (Annualized bps)")
      )
  })
  
  # Select the graph the user wants
  chart_choice_input <- reactive({
    switch(input$chart_choice,
           "duration_chart" = chart_duration(),
           "convexity_chart" = chart_convexity(),
           "delta_chart" = chart_delta(),
           "volatility_chart" = chart_volatility()
    )
  })
  
  # Output to UI
  output$historical_chart <- plotly::renderPlotly({
    chart_choice_input()
  })
  
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
  
  

  
  # Zero rate
  curve_tbl <- reactive({
    df <- x()
    build_curve_tbl(df$data, df$closest_date, grid_times = seq(0.25, 30, by = 0.01), m = 2)
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
  

  
  # -----
  # green portfolio section
  # -----
  
  
  portfolio_data <- reactiveVal(
    tibble::tibble(
      bond = character(),
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
      bond_id = NA_character_,
      Date = input$issue_date,
      Quantity = input$quantity_input,
      Maturity = input$maturity_input,
      Rate = input$rate_input,
      Price = input$price_input
    )
    
    bond_label <- dplyr::bind_rows(portfolio_data(), new_row)
    bond_label$bond_id <- paste0("Bond ", 1:nrow(bond_label))
    portfolio_data(bond_label)
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
    bond_label <- portfolio_data()[-input$portfolio_table_rows_selected, ]
    
    if(nrow(bond_label) > 0) {
      bond_label$bond_id <- paste0("Bond ", 1:nrow(bond_label))
    }
    portfolio_data(bond_label)
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
      selection = "multiple",
      editable = list(
        target = 'cell',
        disable = list(columns = 0)),
      options = list(
        searching = FALSE,
        lengthchange = FALSE,
        paging = FALSE,
        info = FALSE)
    )
  })
  
  
  
  
  # Calculating Portfolio Value from user inputs
  
  portfolio_value <- reactive({
    
    tryCatch({
      
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
      
      
      total_value <- calculate_portfolio(d,ct,val_date)
      
      list(price = total_value)
    }, error = function(error) {
      return(NULL)
    })
  }
  ) %>% bindEvent(input$calculate_value)
  
  output$portfolio_value <- renderText({
    req(input$calculate_value)
    val <- portfolio_value()$price
    
    req(!is.null(val))
    paste0("$", formatC(val, format = "f", digits = 2, big.mark = ","))
    
  })
  
  
  port_duration <- reactive({
    
    tryCatch({
      
      c <- 0.0001
      
      d <- portfolio_data()
      req(nrow(d)>0)
      
      val_date <- input$portfolio_valuation_date
      req(!is.null(val_date))
      
      val_curve_data_up <- cmt_clean %>% 
        filter(as.Date(date) == as.Date(val_date)) %>% 
        dplyr::mutate(rate = rate + c)
      
      if (nrow(val_curve_data_up) == 0) {
        available_dates <- sort(unique(as.Date(cmt_clean$date)))
        closest <- available_dates[which.min(abs(available_dates - as.Date(val_date)))]
        val_curve_data <- cmt_clean %>% filter(as.Date(date) == closest)
        val_date <- closest
      }
      
      ct_up <- build_curve_tbl(val_curve_data_up, val_date)
      
      
      val_curve_data_dwn <- cmt_clean %>% 
        filter(as.Date(date) == as.Date(val_date)) %>% 
        dplyr::mutate(rate = rate - c)
      
      
      if (nrow(val_curve_data_dwn) == 0) {
        available_dates <- sort(unique(as.Date(cmt_clean$date)))
        closest <- available_dates[which.min(abs(available_dates - as.Date(val_date)))]
        val_curve_data <- cmt_clean %>% filter(as.Date(date) == closest)
        val_date <- closest
      }
      
      ct_dwn <- build_curve_tbl(val_curve_data_dwn, val_date)
      
      
      value_up <- calculate_portfolio(d,ct_up,val_date)
      value_dwn <- calculate_portfolio(d,ct_dwn,val_date)
      
      
      num_duration <-  (value_up - value_dwn) / (2 * c) / portfolio_value()$price
      num_gamma <- 0.5 * ((value_up - 2 * portfolio_value()$price + value_dwn) / c^2) / 10000^2
      
      DV_01_dur <- c * num_duration * portfolio_value()$price
      
      
      list(delta = num_duration,
           gamma = num_gamma,
           DV01_dur = DV_01_dur)
    },
    error = function(error) {
      return(NULL)
    })
  }) %>% bindEvent(input$calculate_value)
  
  
  bond_dv01 <- reactive({
    
    tryCatch({
    req(input$calculate_value)
    c <- 0.0001
    
    d <- portfolio_data()
    req(nrow(d)>0)
    
    val_date <- input$portfolio_valuation_date
    req(!is.null(val_date))
    
    val_curve_data_up <- cmt_clean %>% 
      filter(as.Date(date) == as.Date(val_date)) %>% 
      dplyr::mutate(rate = rate + c)
    
    val_curve_data_dwn <- cmt_clean %>% 
      filter(as.Date(date) == as.Date(val_date)) %>% 
      dplyr::mutate(rate = rate - c)
    
    ct_up <- build_curve_tbl(val_curve_data_up, val_date)
    ct_dwn <- build_curve_tbl(val_curve_data_dwn, val_date)
    
    dv01 <- tibble::tibble()
    
    for (bond in 1:nrow(d)) {
      
      value_up <- calculate_portfolio(d[bond,],ct_up,val_date)
      value_dwn <- calculate_portfolio(d[bond,],ct_dwn,val_date)
      
      dv01_bond <- (value_up - value_dwn) / (2 * c) / 10000
      
      dv01 <- dplyr::bind_rows(
        dv01,
        tibble::tibble(
          bond_id = d$bond_id[bond],
          DV01 = dv01_bond))
      
    }
    
    total_dv01 <- sum(abs(dv01$DV01))
    
    dv01 %>%
      dplyr::mutate(
        DV01_pct = abs(DV01) / total_dv01)
  }, error = function(error) {
    shiny::showNotification(
      paste0("Cannot calculate portolio value"),
      type = "error"
    )
    return(NULL)
    
  })
}) %>% bindEvent(input$calculate_value)
  
  
  output$port_pie <- renderPlotly({
    
    req(input$calculate_value)
    df <- bond_dv01()
    
    req(!is.null(df),nrow(df)>0)
    plotly::plot_ly(
      data = df,
      labels = ~bond_id,
      values = ~DV01_pct,
      type = "pie") %>% 
      layout(
        title = "DV01 Contribution to Portfolio",
        showlegend = FALSE)
    
  })
  
  
  output$port_duration <- renderText({
    req(input$calculate_value)
    dur <-  port_duration()$delta
    req(!is.null(dur))
    paste0(round(dur,4)," Yrs")
    
  })
  
  output$port_convex <- renderText({
    req(input$calculate_value)
    gam <- port_duration()$gamma
    req(!is.null(gam))
    paste0(round(gam,4))
  })
  output$DV_01_delta <- renderText({
    req(input$calculate_value)
    dv1 <- port_duration()$DV01_dur
    req(!is.null(dv1))
    paste0(round(dv1,4))
    
  })
  
  # Scenario analysis section
  
  parallel_shift_data <- reactive({
    req(input$calculate_value)
    
    p_value <- portfolio_value()$price
    req(!is.null(p_value))
    
    p_duration <- port_duration()
    req(!is.null(p_duration))
    
    dollar_dv01 <- p_duration$DV01_dur
    dollar_gamma <- p_duration$gamma
    
    shift_range <- seq(-300,300, by = 10)
    
    tibble::tibble(
      shift_bps = shift_range,
      linear_estimate = p_value + (dollar_dv01 * shift_bps),
      convexity_estimate = p_value + (dollar_dv01 * shift_bps) + (dollar_gamma * (shift_bps^2))
    )
  })
  
  # plotly for parallel shift
  
  
  output$parallel_shift_plot <- plotly::renderPlotly({
    req(input$calculate_value)
    df <- parallel_shift_data()
    req(!is.null(df))
    
    plotly::plot_ly(
      data = df,
      x = ~shift_bps) %>% 
      plotly::add_lines(
        y = ~linear_estimate, 
        name = "Linear Estimate (DV01)",
        line = list(dash = "dash", color = "red")) %>% 
      
      plotly::add_lines(
        y = ~convexity_estimate, 
        name = "Convexity Adjusted Estimate (DV01)",
        line = list(color = "blue")) %>%
      
      plotly::layout(
        title = "Parallel Shift Adjusted Portfolio Value",
        xaxis = list(title = "Yield Shift (bps)"),
        yaxis = list(title = "Estimated Portfolio Value ($)", tickformat = "$,.2f"),
        hovermode = "x unified")
  })
    
  # dt for parallel shift
  
  output$parallel_shift_dt <- DT::renderDataTable({
    req(input$calculate_value)
    df <- parallel_shift_data()
    req(!is.null(df))
    
    df_names <- df %>% 
      dplyr::rename(
        "Shift (bps)" = shift_bps,
        "Linear Estimate (DV01)" = linear_estimate,
        "Convexity Adjusted Estimate (DV01)" = convexity_estimate
      )
    
    DT::datatable(
      df_names,
      rownames = FALSE,
      options = list(
        searching = FALSE,
        lengthchange = FALSE,
        paging = FALSE,
        info = FALSE)
      ) %>% 
      
    DT::formatCurrency(
      columns = c("Linear Estimate (DV01)", "Convexity Adjusted Estimate (DV01)"),
      currency = "$",
      digits = 2
    )
    
  })
    
  
  
}