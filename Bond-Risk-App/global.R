library(tidyquant)
library(tidyverse)
library(RQuantLib)
library(arrow)
library(slider)
library(Rcpp)
Rcpp::sourceCpp("bondcalculator.cpp")

tickers <- c(
  "DGS1MO", # 1-Month
  "DGS3MO", # 3-Month
  "DGS6MO", # 6-Month
  "DGS1",   # 1-Year
  "DGS2",   # 2-Year
  "DGS5",   # 5-Year
  "DGS7",   # 7-Year
  "DGS10",  # 10-Year
  "DGS20",  # 20-Year
  "DGS30"   # 30-Year
)

cmt_clean <- read_feather(
  "feather_data/cmt_clean.feather"
)

risk_result <- cmt_clean %>% 
  dplyr::mutate(
    ytm = rate,
    duration = NA,
    convexity = NA,
    delta = NA,
    gamma = NA
  ) %>% 
  dplyr::select(price, maturity, rate, ytm, m, change_bps, duration, convexity, delta, gamma) %>% 
  as.matrix() %>% 
  bond_risk() %>% 
  as.data.frame()

cmt_clean <- cmt_clean %>% 
  dplyr::mutate(
    duration = risk_result$duration,
    convexity = risk_result$convexity,
    delta = risk_result$delta,
    gamma = risk_result$gamma
  )

order <- c("DGS1MO", # 1-Month
           "DGS3MO", # 3-Month
           "DGS6MO", # 6-Month
           "DGS1",   # 1-Year
           "DGS2",   # 2-Year
           "DGS5",   # 5-Year
           "DGS7",   # 7-Year
           "DGS10",  # 10-Year
           "DGS20",  # 20-Year
           "DGS30"   # 30-Year
)

chart_tickers <- c(
  "DGS1",   # 1-Year
  "DGS2",   # 2-Year
  "DGS5",   # 5-Year
  "DGS7",   # 7-Year
  "DGS10",  # 10-Year
  "DGS20",  # 20-Year
  "DGS30"   # 30-Year
)

chart_data <- cmt_clean %>% 
  dplyr::filter(symbol %in% chart_tickers) %>%
  dplyr::mutate(symbol = factor(symbol, levels = chart_tickers))

volatility_data <- chart_data %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::mutate(
    sd30 = slider::slide_dbl(
      .x = change_bps,
      .f = sd,
      .before = 30,
      .after = 0,
      .complete = TRUE
    ) * sqrt(252)
  ) %>%
  dplyr::ungroup()

# helper function to get the actual values from the ticker names since "DiscountCurve" only reads the numbers
q <- function(df_day, sym){
  df_day %>%
    filter(symbol == sym) %>%
    pull(rate) %>%
    first()
}

# The building process of the zero rate curve requires a function called "DiscountCurve" and it requires some things I create here such as "params" and "tsQuotes"
build_curve_tbl <- function(df_day, eval_date, grid_times = seq(0.25, 30, by = 0.01), m = 2) {
  
  # Quotes for DiscountCurve
  tsQuotes <- list(
    d1m = q(df_day, "DGS1MO"),
    d3m = q(df_day, "DGS3MO"),
    d6m = q(df_day, "DGS6MO"),
    d1y = q(df_day, "DGS1"),
    s2y = q(df_day, "DGS2"),
    s5y = q(df_day, "DGS5"),
    s10y = q(df_day, "DGS10"),
    s20y = q(df_day, "DGS20"),
    s30y = q(df_day, "DGS30")
  )
  
  params <- list(
    tradeDate = eval_date,
    settleDate = eval_date,
    dt = 1/365,
    interpWhat = "discount",
    interpHow = "loglinear"
  )

  # VERY Cool function to get the zero rates
  curve <- RQuantLib::DiscountCurve(params, tsQuotes, grid_times)
  

  
  # Creating a table to show the maturities, DFs and Zero Rates
  tibble(
    maturity = grid_times,
    discount_factor = as.numeric(curve$discounts),
    zero_rate = as.numeric(curve$zerorates)
  )}
  
  # Helper Function to calculate portfolio value
  seq_back <- function(T2M, step) {
    
    T2M <- as.numeric(T2M)
    
    x <- seq(from = T2M, to = 0, by = -step)
    
    remainder <- T2M %% step
    
    if (remainder > 0) {
      x <- c(x[x > remainder], remainder)
    }
    
    return(c(round(x, 2), 0))  
  }
  
  ### Helper Function to calculate portfolio value for delta and gamma calcs
  
  calculate_portfolio <- function(bonds_df, zero_curve, valuation_date) {
    
    total_value <- 0
    
    for (bond in 1:nrow(bonds_df)) {
      
      # Skip if not yet issued
      if (bonds_df$Date[bond] > valuation_date) next
      
      T2M <- as.numeric((bonds_df$Date[bond] + years(bonds_df$Maturity[bond]) - valuation_date) / 365)
      
      # Skip if matured
      if (T2M <= 0) next
      
      cfs <- tibble::tibble(
        maturity = seq_back(T2M = T2M, step = 0.5)
      ) %>% 
        dplyr::arrange(maturity) %>% 
        dplyr::mutate(
          CF = dplyr::case_when(
            maturity == 0 ~ 0,
            dplyr::row_number() == dplyr::n() ~ bonds_df$Price[bond] + (bonds_df$Price[bond] * bonds_df$Rate[bond] / 200),
            TRUE ~ bonds_df$Price[bond] * bonds_df$Rate[bond] / 200
          ),
          maturity = round(maturity, 2)
        ) %>%
        dplyr::left_join(zero_curve, by = "maturity") %>% 
        dplyr::mutate(pv = discount_factor * CF)
      
      bond_value <- sum(cfs$pv, na.rm = TRUE) * bonds_df$Quantity[bond]
      total_value <- total_value + bond_value
    }
    
    total_value
  }
  

