library(testthat)
library(tsbox)

context("automated tests for all supported classes")


test_that("two way conversion", {

  for (class in names(tsbox:::supported_classes())){
    message(class)
    ts_fun <- get(paste0("ts_", class))

    # single series
    expect_equal(ts_ts(ts_fun(AirPassengers)), AirPassengers)

    # non standard regualr
    expect_equal(ts_ts(ts_fun(EuStockMarkets)), EuStockMarkets)

    # mixed frequencies
    expect_equal(ts_ts(ts_fun(ts_c(austres, AirPassengers))), ts_c(austres, AirPassengers))

    # non alphabetical order, multi series
    expect_equal(ts_ts(ts_fun(ts_c(mdeaths, fdeaths))), ts_c(mdeaths, fdeaths))

    # non alphabetical order, multi series
    expect_equal(ts_ts(ts_fun(ts_c(mdeaths, AirPassengers))), ts_c(mdeaths, AirPassengers))
  }

})

