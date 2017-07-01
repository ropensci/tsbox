library(testthat)
library(tsbox)


context("basic conversion handling")

test_that("conversion produces right classes", {
  expect_s3_class(ts_xts(AirPassengers), "xts")
  expect_s3_class(ts_ts(AirPassengers), "ts")
  expect_s3_class(ts_df(AirPassengers), "data.frame")
  expect_s3_class(ts_dt(AirPassengers), "data.table")
  expect_s3_class(ts_tbl(AirPassengers), "tbl_df")

  expect_s3_class(ts_xts(ts_xts(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_xts(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_xts(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_df(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_df(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_df(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_df(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_df(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_dt(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_dt(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_dt(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(AirPassengers)), "tbl_df")

  expect_s3_class(ts_xts(ts_tbl(AirPassengers)), "xts")
  expect_s3_class(ts_ts(ts_tbl(AirPassengers)), "ts")
  expect_s3_class(ts_df(ts_tbl(AirPassengers)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(AirPassengers)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(AirPassengers)), "tbl_df")

})


test_that("conversion between objects works as expected: ldeaths", {

  x.ts <- ts_c(mdeaths, fdeaths)
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)

})


test_that("conversion between objects works as expected: discoveries", {

  x.ts <- discoveries
  x.xts <- ts_xts(discoveries)
  x.df <- ts_df(discoveries)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equivalent(ts_xts(ts_ts(x.xts)), x.xts)  # rownames do not match
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)  
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)   
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)

})


test_that("conversion between objects works as expected: EuStockMarkets", {

  x.ts <- EuStockMarkets
  x.xts <- ts_xts(x.ts)
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)

  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)

  expect_equal(ts_xts(ts_ts(x.xts)), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)

  expect_equal(ts_df(ts_ts(x.df)), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)

  expect_equal(ts_dt(ts_ts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)

  expect_equal(ts_tbl(ts_ts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})



test_that("some trickier situations work properly", {

  # ts_c(
  #     ts_c(AirPassengers, mdeaths),
  #     ts_forecast_mean(ts_c(AirPassengers, mdeaths))
  # )

  # this is a tricky one: a function to detect NAs?
  # ts_rbind(AirPassengers, mdeaths)

})


test_that("2 colum data.frames work as expected", {
  x <- ts_dt(AirPassengers)
  x[, var := NULL]
  ts_dts(x)
})


test_that("selecting and binding works as expected", {

  dta <- ts_df(ts_c(mdeaths, fdeaths))
  expect_equal(mdeaths, ts_ts(ts_select(dta, 'mdeaths')))

})



test_that("colname guessing works as expected", {

  # 3 cols
  library(dplyr)
  x.df <- ts_tbl(ts_c(mdeaths, fdeaths)) %>% 
    setNames(c("Haha", "Hoho", "Hihi"))
  
  x.dt <- as.data.table(x.df)
  expect_equal(mdeaths, ts_select(ts_ts(ts_xts(ts_df(x.df))), 'mdeaths'))
  expect_equal(mdeaths, ts_select(ts_ts(ts_df(ts_xts(ts_ts(x.dt)))), 'mdeaths'))

  # 2 cols
  x.df <- ts_tbl(AirPassengers) %>% 
    select(-var) %>% 
    setNames(c("Haha", "Hoho"))
  
  x.dt <- as.data.table(x.df)
  expect_equal(AirPassengers, ts_select(ts_ts(ts_xts(ts_df(x.df))), 'AirPassengers'))
  expect_equal(AirPassengers, ts_select(ts_ts(ts_df(ts_xts(ts_ts(x.dt)))), 'AirPassengers'))

})

