library(testthat)
library(tsbox)

context("ts_frequency")
test_that("ts_frequency survives freq conversion", {
  expect_equal(
    ts_frequency(EuStockMarkets, 1),
    ts_ts(ts_frequency(ts_xts(EuStockMarkets), 1))
  )
})

test_that("ts_frequency handles na.rm correctly", {

  x <- ts_c(mdeaths, austres)
  window(x, start = c(1985, 6), end = c(1985, 12)) <- NA

  x0 <- ts_frequency(x)
  x1 <- ts_frequency(x, na.rm = TRUE)

  expect_identical(colnames(x0), colnames(x))
  expect_identical(colnames(x1), colnames(x))

  expect_true(is.na(window(x0, start = 1985, end = 1985)[, 'austres']))
  expect_false(is.na(window(x1, start = 1985, end = 1985)[, 'austres']))
})



test_that("ts_frequency works with fancier frequencies", {

  skip_on_cran()

  z <- ts_frequency(EuStockMarkets, to = "week", aggregate = "mean", na.rm = TRUE)
  expect_equal(tail(z, 1)[1], 5414.375)

  expect_is(
    ts_frequency(mdeaths, to = "year", aggregate = "sum", na.rm = TRUE),
    "ts"
  )

  expect_is(
    ts_frequency(mdeaths, to = "month", aggregate = "sum", na.rm = TRUE),
    "ts"
  )

  expect_is(
    ts_frequency(mdeaths, to = "quarter", aggregate = "sum", na.rm = TRUE),
    "ts"
  )

})


test_that("ts_frequency works with POSIXct", {

  x <- tibble(
    time = seq(Sys.time(), length.out = 20, by = "10 sec"),
    value = 1
  )

  expect_is(
    ts_frequency(x, to = "min", aggregate = "sum", na.rm = TRUE),
    "tbl_df"
  )

  expect_is(
    ts_frequency(x, to = "hour", aggregate = "sum", na.rm = TRUE),
    "tbl_df"
  )

})




