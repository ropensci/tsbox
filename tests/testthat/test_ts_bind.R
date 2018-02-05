library(testthat)
library(tsbox)


context("ts_bind")


test_that("ts_bind works as it should.", {
  expect_equal(AirPassengers, 
               ts_bind(ts_window(AirPassengers, start = "1950-01-01"), 
                        ts_window(AirPassengers, end = "1949-12-01"))
               )
  expect_equal(ts_dt(AirPassengers), 
               ts_bind(ts_window(ts_dt(AirPassengers), start = "1950-01-01"), 
                        ts_window(ts_dt(AirPassengers), end = "1949-12-01")))
  expect_equal(ts_df(AirPassengers), ts_bind(AirPassengers = ts_window(ts_df(AirPassengers), start = "1950-01-01"), ts_window(ts_df(AirPassengers), end = "1949-12-01")))
  expect_equal(ts_tbl(AirPassengers), ts_bind(AirPassengers = ts_window(ts_tbl(AirPassengers), start = "1950-01-01"), ts_window(ts_tbl(AirPassengers), end = "1949-12-01")))


  expect_is(ts_bind(ts_dt(mdeaths), AirPassengers), "data.table")

  expect_equal(c(ts_window(ts_bind(mdeaths, 1:10), start = "1980-09-01")), c(9, 10))

})


