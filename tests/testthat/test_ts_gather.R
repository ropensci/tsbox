library(testthat)
library(tsbox)

context("spread gather")


test_that("ts_gather and ts_spread work both ways.", {
  a <- ts_df(ts_c(ts_dt(AirPassengers), mdeaths, fdeaths))
  expect_equal(a, ts_na_omit(ts_gather(ts_spread(a))))

  b <- ts_tbl(ts_dt(EuStockMarkets))
  expect_equal(b, ts_gather(ts_spread(b)))
})
