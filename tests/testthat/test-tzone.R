
library(testthat)
library(tsbox)

#' @srrstats {G5.9b} *Running under different random seeds or initial conditions does not meaningfully change results*
#'   Test that operations do not depend on time zone
test_that("operations do not depend on time zone", {
  skip_on_cran()

  # temp set tz
  old.tz <- Sys.getenv("TZ")
  on.exit(Sys.setenv(TZ = old.tz))
  Sys.setenv(TZ = "America/Los_Angeles")


  expect_equal(
    mdeaths,
    ts_ts(subset(
      ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[, "DAX"]),
      id == "mdeaths"
    ))
  )

  # fails in some time zones and some systems. needs more investigation

  # x <- data.frame(
  #   time = seq(from = as.POSIXct("2000-01-01"), length.out = 10,
  #   by = "1 day"), value = 1:10
  # )
  # expect_equal(x, ts_df(ts_ts(x)))

  # revert to system time zone
  Sys.setenv(TZ = old.tz)
})

test_that("xts and zoo conversion keeps the time zone", {
  skip_if_not_installed("xts")
  tm <- as.POSIXct(
    c("2020-01-01 10:00", "2020-01-01 11:00", "2020-01-01 12:00"),
    tz = "America/New_York"
  )
  x <- data.frame(time = tm, value = 1:3)
  expect_identical(attr(ts_dts(xts::xts(1:3, order.by = tm))$time, "tzone"), "America/New_York")
  expect_equal(ts_df(ts_xts(x)), x)
  expect_equal(ts_df(ts_zoo(x)), x)
})
