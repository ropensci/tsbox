library(testthat)
library(tsbox)

context("dirty situations")

test_that("works with df with improper col classes", {

  library(dplyr)
  x.chr <- ts_tbl(mdeaths) %>%
    mutate(time = as.character(time))

  expect_is(ts_ts(x.chr), "ts")

  x.fct <- ts_tbl(mdeaths) %>%
    mutate(time = as.factor(as.character(time)))

  expect_is(ts_ts(x.fct), "ts")

})



test_that("time column of daily data is treated as Date (#114)", {

  x <- tibble(
    time = seq.Date(as.Date("2000-01-01"), length.out = 10, by = "day"),
    value = rnorm(10)
  )

  z <- ts_dts(ts_ts(x))
  expect_is(z$time, "Date")

})
