library(tidyquant)
library(tidyr)
library(dplyr)
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

from <- "1992-01-01"

cmt_data <- tidyquant::tq_get(
  tickers,
  get = "economic.data",
  from = from) %>% 
  tidyr::fill(price,.direction = "down")


cmt_clean <- cmt_data %>% 
  dplyr::mutate(
    maturity = case_when(
      symbol == "DGS1MO" ~ 1/12,
      symbol == "DGS3MO" ~ 3/12,
      symbol == "DGS6MO" ~ 6/12,
      symbol == "DGS1" ~ 1,
      symbol == "DGS2" ~ 2,
      symbol == "DGS5" ~ 5,
      symbol == "DGS7" ~ 7,
      symbol == "DGS10" ~ 10,
      symbol == "DGS20" ~ 20,
      symbol == "DGS30" ~ 30),
    
    rate = price / 100,
    change_bps = (rate - dplyr::lag(rate)) * 10000,
    m = 2,
    price = 100
  ) %>% 
  tidyr::drop_na()


dir.create("feather_data", recursive = T)
arrow::write_feather(cmt_clean, "feather_data/cmt_clean.feather")
