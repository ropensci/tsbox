library(testthat)
library(tsbox)

context("ts_index")

test_that("ts_index series have same pc rates", {
  expect_equal(
    ts_pc(mdeaths),
    ts_pc(ts_index(mdeaths, "1977-01-01"))
  )

  expect_equal(
    ts_pc(austres),
    ts_pc(ts_index(austres, "1977-01-01"))
  )
})


test_that("ts_index drops errors", {
  expect_error(ts_index(mdeaths, "2000-01-01"))
  expect_error(ts_index(ts_c(mdeaths, fdeaths), "2000-01-01"))
  expect_error(ts_index(EuStockMarkets, "1998-01-01"))
})

test_that("ts_index works with multi ids", {
  x <- bind_rows(
    mutate(ts_tbl(ts_c(fdeaths, mdeaths)), id2 = "one"),
    mutate(ts_tbl(ts_c(fdeaths, mdeaths)), id2 = "two")
  ) %>%
    ts_df() %>%
    ts_tbl()

  expect_equal(
    ts_df(ts_pc(x)),
    ts_df(ts_pc(ts_index(x, "1977-01-01")))
  )
})


test_that("ts_compound works", {
  expect_equal(ts_compound(ts_pc(mdeaths)), ts_index(mdeaths, "1974"))
})
