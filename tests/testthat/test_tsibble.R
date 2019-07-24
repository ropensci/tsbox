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
  weather_tsbl <- as_tsibble(weather, key = origin, index = time_hour)
  weather_tsbl %>%
    ts_default()
})


test_that("tsibbledata sets can be read", {
  skip_if_not_installed("tsibble")
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # slow tests are commented out

  # expect_is(ts_ts(tsibbledata::PBS), "ts")
  # expect_is(ts_ts(tsibbledata::global_economy), "ts")
  expect_is(ts_ts(tsibbledata::ansett), "ts")
  expect_is(ts_ts(tsibbledata::hh_budget), "ts")
  expect_is(ts_ts(tsibbledata::aus_livestock), "ts")
  expect_is(ts_tbl(tsibbledata::nyc_bikes), "tbl_df")
  expect_is(ts_ts(tsibbledata::aus_production), "ts")
  expect_is(ts_ts(tsibbledata::olympic_running), "ts")
  # expect_is(ts_ts(tsibbledata::aus_retail), "ts")
  expect_is(ts_ts(tsibbledata::pelt), "ts")
  expect_is(ts_ts(tsibbledata::gafa_stock), "ts")
  expect_is(ts_ts(tsibbledata::vic_elec), "ts")
})
