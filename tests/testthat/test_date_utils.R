library(testthat)
library(tsbox)

context("date utils")


test_that("df aggregation using date_ functions is working", {

  # 3 cols
  library(dplyr)
  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = date_year(time)) %>% 
    group_by(id, time) %>% 
    summarize(value = mean(value)) %>% 
    ungroup()

  expect_equal(x, ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "year")))


  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = date_quarter(time)) %>% 
    group_by(id, time) %>% 
    summarize(value = mean(value)) %>% 
    ungroup()

  expect_equal(x, ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "quarter")))


  x <- ts_tbl(ts_c(EuStockMarkets)) %>%
    mutate(time = date_month(time)) %>% 
    group_by(id, time) %>% 
    summarize(value = mean(value)) %>% 
    ungroup()

  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(EuStockMarkets), "month")), id))

})


test_that("date_shift is working", {

  x <- ts_tbl(ts_c(mdeaths, fdeaths))  
  expect_equal(x$time, date_shift(x$time))

  x1 <- ts_df(ts_c(mdeaths, fdeaths)) %>% 
    mutate(time = date_shift(time, by = "month")) %>% 
    ts_window(end = "1979-12-01")
  xlag <- ts_lag(x)

  expect_equal(xlag, x1)

})
