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


context("ts_lag")

test_that("ts_lag works as stats::lag", {
  expect_equal(
    ts_lag(mdeaths),
    ts_window(stats::lag(mdeaths, -1), end = "1979-12-01")
  )

  expect_equal(
    ts_lag(mdeaths, -1),
    ts_window(stats::lag(mdeaths, 1), start = "1974-01-01")
  )
})

test_that("ts_lag works both ways", {
  expect_equal(
    ts_lag(ts_lag(mdeaths, -1)),
    ts_lag(ts_lag(mdeaths), -1)
  )
})

