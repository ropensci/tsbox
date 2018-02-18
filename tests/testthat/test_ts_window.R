library(testthat)
library(tsbox)

context("ts_window")


test_that("ts_window does the same as base window", {
  ts_win2 <- ts_(window)
  expect_equal(
    ts_win2(ts_c(mdeaths, fdeaths), start = c(1978, 3)),
    ts_window(ts_c(mdeaths, fdeaths), start = "1978-03-01")
  )

  expect_equal(
    ts_win2(ts_c(austres), start = c(1978, 2)),
    ts_window(ts_c(austres), start = "1978-04-01")
  )

  expect_equal(
    ts_win2(ts_c(mdeaths, fdeaths), end = c(1978, 3)),
    ts_window(ts_c(mdeaths, fdeaths), end = "1978-03-01")
  )

  expect_equal(
    ts_win2(ts_c(austres), end = c(1978, 2)),
    ts_window(ts_c(austres), end = "1978-04-01")
  )

})
