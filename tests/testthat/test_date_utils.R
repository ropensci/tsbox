library(testthat)
library(tsbox)

context("date utils")


test_that("df aggregation using first_time_of_ functions is working", {

  # 3 cols
  library(dplyr)
  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = first_time_of_year(time)) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup()

  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "year")), id))


  x <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = first_time_of_quarter(time)) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup()

  expect_equal(x, ts_tbl(ts_frequency(ts_c(mdeaths, fdeaths), "quarter")))


  x <- ts_tbl(ts_c(EuStockMarkets)) %>%
    mutate(time = as.Date(first_time_of_month(time))) %>%
    group_by(id, time) %>%
    summarize(value = mean(value)) %>%
    ungroup()

  expect_equal(x, arrange(ts_tbl(ts_frequency(ts_c(EuStockMarkets), "month")), id))
})


test_that("time_shift is working", {
  x <- ts_tbl(ts_c(mdeaths, fdeaths))
  expect_equal(x$time, time_shift(x$time))

  x1 <- ts_df(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = time_shift(time, by = "month")) 
  xlag <- ts_lag(x)

  expect_equal(xlag, x1)
})
