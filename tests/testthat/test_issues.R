library(testthat)
library(tsbox)

context("tslist")
test_that("ts_trend does retransform (#193)", {
  ans <- ts_trend(AirPassengers)
  expect_s3_class(ans, "ts")
})

