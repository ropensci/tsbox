library(testthat)
library(tsbox)

context("tslist")
test_that("ts_trend does retransform (#193)", {
  ans <- ts_trend(AirPassengers)
  expect_s3_class(ans, "ts")
})


context("zoo and xts")
test_that("regular zoo and xts can be processed", {
  library(zoo)
  z <- as.zoo(USAccDeaths)
  expect_is(ts_tbl(z), "tbl")
  expect_is(ts_tbl(as.xts(z)), "tbl")
})
