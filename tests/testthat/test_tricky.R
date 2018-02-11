library(testthat)
library(tsbox)

# Major Things

# - [ ] ts_bind, ts_c, binding POSIXct and Data should result in POSIXct,
#       also should have the same var naming behavior)
# perhaps this can be done by letting ts_bind calling ts_c

# - [ ] reclassing of numeric, matrix output, similar to xts::reclass

# ts_scale <- ts_(function(x, ...){
#   z <- scale.default(unclass(x), ...)
#   xts::reclass(z, x)
# }, class = "xts")

# - [ ] a cleaner ts_ function
# - [ ] Lookup table for heuristic frequency conversion
# - [ ] numeric vectors in ts_window()
# - [ ] Tools to enforce regularity (major and quite separate task)

context("tricky stuff")

ts_dygraphs(AirPassengers)

test_that("Latest tricky stuff works.", {
  expect_equal(
    mdeaths,
    ts_ts(subset(
      ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"]),
      id == "mdeaths"
    ))
  )

  # names must be unique!!
  a <- ts_dts(ts_c(AirPassengers, AirPassengers))
  expect_true(length(unique(a[["id"]])) == 2)

  # ts_c for ts objects
  expect_is(ts_c(ts_c(fdeaths, mdeaths), AirPassengers), "ts")
})






test_that("Some trickier stuff works.", {
  expect_s3_class(ts_c(EuStockMarkets, mdeaths, fdeaths), "data.frame")

  x <- ts_c(ts_df(ts_c(mdeaths, fdeaths)), AirPassengers)
  expect_equal(ts_ts(subset(x, id == "AirPassengers")), AirPassengers)

  # series of length 2
  a <- ts_dts(window(AirPassengers, end = c(1949, 2)))
  ts_ts(a)
})



test_that("No Invalid .internal.selfref detected.", {
  x <- ts_dts(AirPassengers)
  expect_silent(x[, s := "sdfsd"])
})


test_that("Unordered time works", {
  suppressMessages(library(dplyr))
  ap.rev <- arrange(AirPassengers, desc(time)) 

  expect_equal(ts_ts(ap.rev), AirPassengers)
  expect_equal(ts_ts(ts_diff(ap.rev)), ts_diff(AirPassengers))
})

