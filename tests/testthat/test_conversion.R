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


test_that("conversion between objects works as expected: ldeaths", {

  x.ts <- ts_bind(mdeaths, fdeaths)
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)

})


test_that("conversion between objects works as expected: discoveries", {

  x.ts <- discoveries
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(unname(ts_xts(ts_df(x.xts))), x.xts)
  expect_equal(unname(ts_xts(ts_dt(x.xts))), x.xts)
  expect_equal(unname(ts_xts(ts_tbl(x.xts))), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)

})


test_that("conversion between objects works as expected: EuStockMarkets", {

  x.ts <- EuStockMarkets
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})



test_that("some trickier situations work properly", {

  ts_bind(
      ts_bind(AirPassengers, mdeaths),
      ts_forecast(ts_bind(AirPassengers, mdeaths))
  )

  # this is a tricky one: a function to detect NAs?
  # ts_rbind(AirPassengers, mdeaths)

})







test_that("selecting and binding works as expected", {

  dta <- ts_df(ts_bind(mdeaths, fdeaths))
  expect_equal(mdeaths, ts_ts(ts_select(dta, 'mdeaths')))

})



test_that("selecting and binding works as expected", {
  # manipulation of col names do not affect calculations

  op <- options(tsbox.var.name = "Haha",
                tsbox.time.name = "Hoho",
                tsbox.value.name = "Hihi")
  on.exit(options(op))
  dta <- ts_df(ts_dt(ts_xts(ts_bind(mdeaths, fdeaths))))
  expect_equal(mdeaths, ts_ts(ts_dt(ts_select(dta, 'mdeaths'))))
  expect_equal(ts_ts(ts_select(ts_tbl(ts_bind(fdeaths, mdeaths)), "mdeaths")), mdeaths)
})







