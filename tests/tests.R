

library(testthat)


context("data time conversion")

test_that("two way conversion", {
  expect_equal(POSIXct_to_tsp(ts_to_POSIXct(AirPassengers)), tsp(AirPassengers))
  expect_equal(POSIXct_to_tsp(ts_to_POSIXct(EuStockMarkets)), tsp(EuStockMarkets))
  expect_equal(POSIXct_to_tsp(ts_to_POSIXct(discoveries)), tsp(discoveries))
  expect_equal(POSIXct_to_tsp(ts_to_POSIXct(mdeaths)), tsp(mdeaths))
  expect_equal(POSIXct_to_tsp(ts_to_POSIXct(uspop)), tsp(uspop))
  expect_equal(POSIXct_to_tsp(ts_to_POSIXct(austres)), tsp(austres))
})



test_that("the test works at all", {
  expect_true(FALSE)
})
