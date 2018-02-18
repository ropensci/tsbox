library(testthat)
library(tsbox)

context("ts_pc")


test_that("colname guessing works as expected", {

  # 3 cols
  library(dplyr)
  x.df <- ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    setNames(c("Haha", "Hoho", "Hihi"))

  expect_equal(ts_pc(mdeaths), ts_ts(ts_xts(ts_df(ts_pc(x.df))))[, "mdeaths"])
  expect_equal(ts_diff(mdeaths), ts_ts(ts_xts(ts_df(ts_diff(x.df))))[, "mdeaths"])
  expect_equal(ts_pcy(mdeaths), ts_ts(ts_xts(ts_df(ts_pcy(x.df))))[, "mdeaths"])
  expect_equal(ts_diffy(mdeaths), ts_ts(ts_xts(ts_df(ts_diffy(x.df))))[, "mdeaths"])

  # 2 cols
  x.df <- ts_tbl(AirPassengers) %>%
    setNames(c("Haha", "Hoho"))

  expect_equal(ts_pc(AirPassengers), ts_ts(ts_xts(ts_df(ts_pc(x.df)))))
  expect_equal(ts_diff(AirPassengers), ts_ts(ts_xts(ts_df(ts_diff(x.df)))))
  expect_equal(ts_pcy(AirPassengers), ts_ts(ts_xts(ts_df(ts_pcy(x.df)))))
  expect_equal(ts_diffy(AirPassengers), ts_ts(ts_xts(ts_df(ts_diffy(x.df)))))
})


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
