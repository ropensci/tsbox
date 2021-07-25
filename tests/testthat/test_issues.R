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
  expect_is(ts_tbl(xts::as.xts(z)), "tbl")
})

test_that("ensure time is always ordered (#202)", {
  unorderd <- ts_tbl(ts(1:3, start = 2000))[c(2, 3, 1), ]
  expect_true(all(diff(ts_tbl(unorderd)$time) > 0))
})
