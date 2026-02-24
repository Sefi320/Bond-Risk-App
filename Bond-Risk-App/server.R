#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(bslib)
library(corrplot)

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
  
  
  
  
  
  
  
  
}
