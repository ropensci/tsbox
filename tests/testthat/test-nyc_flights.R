library(testthat)
library(tsbox)

test_that("minimal example works", {
  skip_if_not_installed("nycflights13")

  library(dplyr)
  library(nycflights13)
  dta <- weather %>%
    select(origin, time = time_hour, temp, humid, precip) %>%
    ts_long()

  expect_s3_class(dta, "tbl_df")
  expect_s3_class(ts_ts(ts_pc(dta)), "ts")
})
