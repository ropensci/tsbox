library(testthat)
library(data.table) 
library(tsbox)
library(dplyr)
library(tsbox)

context("data time conversion")

test_that("two way date time conversion", {
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(AirPassengers)), tsp(AirPassengers))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(EuStockMarkets)), tsp(EuStockMarkets))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(discoveries)), tsp(discoveries))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(mdeaths)), tsp(mdeaths))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(uspop)), tsp(uspop))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(austres)), tsp(austres))
})

