library(testthat)
library(tsbox)


context("ts_bind")


test_that("ts_bind works as it should.", {
  expect_equal(
    AirPassengers,
    ts_bind(
      ts_window(AirPassengers, start = "1950-01-01"),
      ts_window(AirPassengers, end = "1949-12-01")
    )
  )
  expect_equal(
    ts_dt(AirPassengers),
    ts_bind(
      ts_window(ts_dt(AirPassengers), start = "1950-01-01"),
      ts_window(ts_dt(AirPassengers), end = "1949-12-01")
    )
  )
  expect_equal(ts_df(AirPassengers), ts_bind(AirPassengers = ts_window(ts_df(AirPassengers), start = "1950-01-01"), ts_window(ts_df(AirPassengers), end = "1949-12-01")))
  expect_equal(ts_tbl(AirPassengers), ts_bind(AirPassengers = ts_window(ts_tbl(AirPassengers), start = "1950-01-01"), ts_window(ts_tbl(AirPassengers), end = "1949-12-01")))


  expect_is(ts_bind(ts_dt(mdeaths), AirPassengers), "data.table")

  expect_equal(c(ts_window(ts_bind(mdeaths, 1:10), start = "1980-09-01")), c(9, 10))
})


test_that("ts_chain gives correct results", {
  x <- ts_chain(
    ts_window(mdeaths, start = "1975-01-01", end = "1975-12-01"),
    fdeaths
  )

  expect_equal(sum(ts_window((ts_pc(x) - ts_pc(fdeaths)), start = "1976-01-01")), 0)
  expect_equal(sum(ts_window((ts_pc(x) - ts_pc(fdeaths)), end = "1974-12-01")), 0)

  x.df <- ts_chain(
    ts_window(ts_df(mdeaths), start = "1975-01-01", end = "1975-12-01"),
    ts_df(fdeaths)
  )
  x.xts <- ts_chain(
    ts_window(ts_xts(mdeaths), start = "1975-01-01", end = "1975-12-01"),
    ts_xts(fdeaths)
  )
  x.tbl <- ts_chain(
    ts_window(ts_tbl(mdeaths), start = "1975-01-01", end = "1975-12-01"),
    ts_tbl(fdeaths)
  )

  expect_equal(x.df, ts_df(x))
  expect_equal(x.xts, ts_xts(x))
  expect_equal(x.tbl, ts_tbl(x))
})



#' ts_chain(ts_window(mdeaths, end = "1975-12-01"), fdeaths)
#'
