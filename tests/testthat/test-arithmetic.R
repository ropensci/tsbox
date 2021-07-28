
library(testthat)
library(tsbox)


test_that("arithmetic operations work properly", {

  expect_equal(
    fdeaths + mdeaths,
    ts_ts(ts_df(fdeaths) %ts+% mdeaths)
  )

  expect_equal(
    fdeaths - mdeaths,
    ts_ts(ts_df(fdeaths) %ts-% mdeaths)
  )

  expect_equal(
    fdeaths * mdeaths,
    ts_ts(ts_df(fdeaths) %ts*% mdeaths)
  )

  expect_equal(
    fdeaths / mdeaths,
    ts_ts(ts_df(fdeaths) %ts/% mdeaths)
  )



  # functional test
  library(dplyr)
  a <- bind_rows(
    mutate(ts_tbl(ts_c(mdeaths, fdeaths)), id2 = "a"),
    mutate(ts_tbl(ts_c(mdeaths, fdeaths)), id2 = "b")
  )
  expect_is(a %ts/% a, "data.frame")

  z <- ts_df(ts_c(mdeaths, fdeaths) %ts/% 1)
  expect_is(z, "data.frame")






})
