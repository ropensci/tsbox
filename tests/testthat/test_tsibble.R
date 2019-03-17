library(testthat)
library(tsbox)

# install.packages(c("tsibble", "xts", "timeSeries", "zoo", "tibbletime"))

context("handling of tsibbles")


test_that("two way conversion works for tsibbles, too.", {
  skip_if_not_installed("tsibble")
  library(tsibble)

  # tsibble alphabetically reorders key column
  # mixed frequencies
  expect_equal(ts_ts(ts_tsibble(ts_c(austres, AirPassengers))), ts_c(AirPassengers, austres))
  # non alphabetical order, multi series
  expect_equal(ts_ts(ts_tsibble(ts_c(mdeaths, fdeaths))), ts_c(fdeaths, mdeaths))
  # non alphabetical order, multi series
  expect_equal(ts_ts(ts_tsibble(ts_c(mdeaths, AirPassengers))), ts_c(AirPassengers, mdeaths))

})



test_that("tsibble back-conversion works properly", {

  skip_if_not_installed("nycflights13")
  skip_if_not_installed("tsibble")
  library(tsibble)

  weather <- nycflights13::weather %>%
    select(origin, time_hour, temp, humid, precip)
  weather_tsbl <- as_tsibble(weather, key = id(origin), index = time_hour)
  weather_tsbl %>%
    ts_default()
})

