library(testthat)
library(tsbox)

context("ts_shift")


test_that("ts_shift works", {

  expect_equal(
    ts_lag(mdeaths),
    ts_window(ts_shift(mdeaths, "month"), end = ts_end(mdeaths))
  )

  expect_equal(
    ts_lag(austres),
    ts_window(ts_shift(austres, "quarter"), end = ts_end(austres))
  )

})
