library(testthat)
library(data.table) 
library(tsbox)
library(dplyr)
library(tsbox)

context("data time conversion")

test_that("two way conversion", {
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(AirPassengers)), tsp(AirPassengers))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(EuStockMarkets)), tsp(EuStockMarkets))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(discoveries)), tsp(discoveries))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(mdeaths)), tsp(mdeaths))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(uspop)), tsp(uspop))
  expect_equal(tsbox:::POSIXct_to_tsp(tsbox:::ts_to_POSIXct(austres)), tsp(austres))
})


test_that("conversion between objects works as expected", {

  x.ts <- tsbind(mdeaths, fdeaths)
  x.xts <- as_xts(x.ts)
  x.df <- as_df(x.xts)
  x.dt <- as_dt(x.df)

  expect_equal(as_ts(as_xts(x.ts)), x.ts)
  expect_equal(as_ts(as_df(x.ts)), x.ts)
  expect_equal(as_ts(as_dt(x.ts)), x.ts)

  expect_equal(as_xts(as_ts(x.xts)), x.xts)
  expect_equal(as_xts(as_df(x.xts)), x.xts)
  expect_equal(as_xts(as_dt(x.xts)), x.xts)

  expect_equal(as_df(as_ts(x.df)), x.df)
  expect_equal(as_df(as_xts(x.df)), x.df)
  expect_equal(as_df(as_dt(x.df)), x.df)

  expect_equal(as_dt(as_ts(x.dt)), x.dt)
  expect_equal(as_dt(as_xts(x.dt)), x.dt)
  expect_equal(as_dt(as_df(x.dt)), x.dt)

})



test_that("some trickier situations work properly", {

  tsbind(
      tsbind(AirPassengers, mdeaths),
      tsforecast(tsbind(AirPassengers, mdeaths))
  )

  # this is a tricky one: a function to detect NAs?
  # tsrbind(AirPassengers, mdeaths)

})




test_that("examples from README.md work properly", {

  x.ts <- tsbind(mdeaths, fdeaths)
  x.xts <- as_xts(x.ts)
  x.df <- as_df(x.xts)
  x.dt <- as_dt(x.df)

  tsscale(x.ts)  # normalization
  tsscale(x.xts)
  tsscale(x.df)
  tsscale(x.dt)

  tstrend(x.ts)  # loess trend line
  tspc(x.ts)
  tspcy(x.ts)
  tslag(x.ts)
  tsprcomp(tsbind(mdeaths, fdeaths))  # first principal component

  # with external packages
  tsforecast(x.ts)  # ets forecast
  # tsseas(x.ts)  # X-13 seasonal adjustment

  tsbind(as_dt(EuStockMarkets), AirPassengers)
  tsbind(EuStockMarkets, mdeaths)

  tsrbind(as_dt(mdeaths), AirPassengers)
  tsrbind(as_xts(AirPassengers), mdeaths)

  tsplot(tsscale(tsbind(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX'])))
  tsggplot(tsscale(tsbind(discoveries, austres, AirPassengers)))


  dta <- as_df(tsbind(mdeaths, fdeaths))

  dta %>%
    tsbind(lmdeaths = tslag(tsselect(dta, 'mdeaths'), -1)) %>%
    tspredictlm(mdeaths ~ lmdeaths + fdeaths) %>%
    tsplot()


})


