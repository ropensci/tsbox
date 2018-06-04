
library(testthat)
library(tsbox)

old.tz <- Sys.getenv("TZ")
on.exit(Sys.setenv(TZ = old.tz))

test_that("ts_ts is time zone independent", {
  expect_equal(
    mdeaths,
    ts_ts(subset(
      ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"]),
      id == "mdeaths"
    ))
  )
})

test_that("daily 2 way conversion is time zone independent", {
  x <- data.frame(
    time = seq(from = as.POSIXct("2000-01-01"), length.out = 10, by = "1 day"),
    value = 1:10
  )
  expect_equal(x, ts_df(ts_ts(x)))
})

Sys.setenv(TZ = old.tz)
