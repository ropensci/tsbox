
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


})
