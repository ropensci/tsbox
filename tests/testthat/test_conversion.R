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

  x.ts <- tsbind(mdeaths, fdeaths)
  x.xts <- as_xts(x.ts)
  x.df <- as_df(x.xts)
  x.dt <- as_dt(x.df)
  x.tbl <- as_tbl(x.dt)

  expect_equal(as_ts(as_xts(x.ts)), x.ts)
  expect_equal(as_ts(as_df(x.ts)), x.ts)
  expect_equal(as_ts(as_dt(x.ts)), x.ts)
  expect_equal(as_ts(as_tbl(x.ts)), x.ts)

  expect_equal(as_xts(as_ts(x.xts)), x.xts)
  expect_equal(as_xts(as_df(x.xts)), x.xts)
  expect_equal(as_xts(as_dt(x.xts)), x.xts)
  expect_equal(as_xts(as_tbl(x.xts)), x.xts)

  expect_equal(as_df(as_ts(x.df)), x.df)
  expect_equal(as_df(as_xts(x.df)), x.df)
  expect_equal(as_df(as_dt(x.df)), x.df)
  expect_equal(as_df(as_tbl(x.df)), x.df)

  expect_equal(as_dt(as_ts(x.dt)), x.dt)
  expect_equal(as_dt(as_xts(x.dt)), x.dt)
  expect_equal(as_dt(as_df(x.dt)), x.dt)
  expect_equal(as_dt(as_tbl(x.dt)), x.dt)

  expect_equal(as_tbl(as_ts(x.tbl)), x.tbl)
  expect_equal(as_tbl(as_xts(x.tbl)), x.tbl)
  expect_equal(as_tbl(as_df(x.tbl)), x.tbl)
  expect_equal(as_tbl(as_dt(x.tbl)), x.tbl)

})


test_that("conversion between objects works as expected: discoveries", {

  x.ts <- discoveries
  x.xts <- as_xts(x.ts)
  x.df <- as_df(x.xts)
  x.dt <- as_dt(x.df)
  x.tbl <- as_tbl(x.dt)

  expect_equal(as_ts(as_xts(x.ts)), x.ts)
  expect_equal(as_ts(as_df(x.ts)), x.ts)
  expect_equal(as_ts(as_dt(x.ts)), x.ts)
  expect_equal(as_ts(as_tbl(x.ts)), x.ts)

  expect_equal(as_xts(as_ts(x.xts)), x.xts)
  expect_equal(unname(as_xts(as_df(x.xts))), x.xts)
  expect_equal(unname(as_xts(as_dt(x.xts))), x.xts)
  expect_equal(unname(as_xts(as_tbl(x.xts))), x.xts)

  expect_equal(as_df(as_ts(x.df)), x.df)
  expect_equal(as_df(as_xts(x.df)), x.df)
  expect_equal(as_df(as_dt(x.df)), x.df)
  expect_equal(as_dt(as_tbl(x.dt)), x.dt)

  expect_equal(as_dt(as_ts(x.dt)), x.dt)
  expect_equal(as_dt(as_xts(x.dt)), x.dt)
  expect_equal(as_dt(as_df(x.dt)), x.dt)
  expect_equal(as_tbl(as_dt(x.tbl)), x.tbl)

})


test_that("conversion between objects works as expected: EuStockMarkets", {

  x.ts <- EuStockMarkets
  x.xts <- as_xts(x.ts)
  x.df <- as_df(x.xts)
  x.dt <- as_dt(x.df)
  x.tbl <- as_tbl(x.dt)

  expect_equal(as_ts(as_xts(x.ts)), x.ts)
  expect_equal(as_ts(as_df(x.ts)), x.ts)
  expect_equal(as_ts(as_dt(x.ts)), x.ts)
  expect_equal(as_ts(as_tbl(x.ts)), x.ts)

  expect_equal(as_xts(as_ts(x.xts)), x.xts)
  expect_equal(as_xts(as_df(x.xts)), x.xts)
  expect_equal(as_xts(as_dt(x.xts)), x.xts)
  expect_equal(as_xts(as_tbl(x.xts)), x.xts)

  expect_equal(as_df(as_ts(x.df)), x.df)
  expect_equal(as_df(as_xts(x.df)), x.df)
  expect_equal(as_df(as_dt(x.df)), x.df)
  expect_equal(as_df(as_tbl(x.df)), x.df)

  expect_equal(as_dt(as_ts(x.dt)), x.dt)
  expect_equal(as_dt(as_xts(x.dt)), x.dt)
  expect_equal(as_dt(as_df(x.dt)), x.dt)
  expect_equal(as_dt(as_tbl(x.dt)), x.dt)

  expect_equal(as_tbl(as_ts(x.tbl)), x.tbl)
  expect_equal(as_tbl(as_xts(x.tbl)), x.tbl)
  expect_equal(as_tbl(as_df(x.tbl)), x.tbl)
  expect_equal(as_tbl(as_dt(x.tbl)), x.tbl)
})



test_that("some trickier situations work properly", {

  tsbind(
      tsbind(AirPassengers, mdeaths),
      tsforecast(tsbind(AirPassengers, mdeaths))
  )

  # this is a tricky one: a function to detect NAs?
  # tsrbind(AirPassengers, mdeaths)

})







test_that("selecting and binding works as expected", {

  dta <- as_df(tsbind(mdeaths, fdeaths))
  expect_equal(mdeaths, as_ts(tsselect(dta, 'mdeaths')))

})



test_that("selecting and binding works as expected", {
  # manipulation of col names do not affect calculations

  op <- options(tsbox.var.name = "Haha",
                tsbox.time.name = "Hoho",
                tsbox.value.name = "Hihi")
  on.exit(options(op))
  dta <- as_df(as_dt(as_xts(tsbind(mdeaths, fdeaths))))
  expect_equal(mdeaths, as_ts(as_dt(tsselect(dta, 'mdeaths'))))
  expect_equal(as_ts(tsselect(as_tbl(tsbind(fdeaths, mdeaths)), "mdeaths")), mdeaths)
})







