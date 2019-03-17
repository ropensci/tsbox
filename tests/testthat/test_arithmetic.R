
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
  z <- ts_df(ts_c(mdeaths, fdeaths) %ts/% ts_c(mdeaths, fdeaths))
  expect_is(z, "data.frame")
  z <- ts_ts(ts_df(ts_c(mdeaths, fdeaths) %ts/% 1))
  expect_is(z, "data.frame")

  )


})
