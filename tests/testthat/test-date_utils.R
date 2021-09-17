library(testthat)
library(tsbox)
library(dplyr)

test_that("time_shift is working", {
  x <- ts_tbl(ts_c(mdeaths, fdeaths))
  expect_equal(x$time, tsbox:::time_shift(x$time))

  x1 <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(time = tsbox:::time_shift(time, by = "month"))
  xlag <- ts_lag(x)

  expect_equal(xlag, x1)
})


test_that("non heuristic reguarization works for Date", {
  x <- as.Date(c(
    "2001-01-02", "2001-01-04", "2001-01-06", "2001-01-08",
    "2001-01-10", "2001-01-14"
  ))
  expect_s3_class(regularize_non_heuristic(x), "Date")
})

test_that("time shift works in special situations", {
  z <- time_shift(
    c(
      seq(as.POSIXct("2001-01-01"), as.POSIXct("2001-01-02"), by = "hour"),
      as.POSIXct("2001-01-02 00:02:11 CET")
    ),
    by = "hour"
  )
  expect_s3_class(z, "POSIXct")
})


test_that("find_range() utility works", {
  expect_type(find_range("month"), "double")
})



# test_that("time zones are not removed", {
#   x <- ts_tbl(EuStockMarkets)
#   attr(x$time, "tzone") <- "UTC"
#   # ts_pc(x)
# })
