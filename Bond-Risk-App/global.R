library(tidyquant)
library(tidyverse)
library(RQuantLib)
library(arrow)

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

# helper function to get the actual values from the ticker names since "DiscountCurve" only reads the numbers
q <- function(df_day, sym){
  df_day %>%
    filter(symbol == sym) %>%
    pull(rate) %>%
    first()
}

# The building process of the zero rate curve requires a function called "DiscountCurve" and it requires some things I create here such as "params" and "tsQuotes"
build_curve_tbl <- function(df_day, eval_date, grid_times = seq(0.25, 30, by = 0.25), m = 2) {
  
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
  )
  
}



