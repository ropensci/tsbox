library(testthat)
library(tsbox)

t3 <- ts_rbind(ts_dt(mdeaths), AirPassengers)   ##### this function contains error, should have both
# AirPassengers and mdeaths in column "var"
t4 <- ts_rbind(ts_xts(AirPassengers), ts_tbl(mdeaths)) ##### this function contains error, should have both
# AirPassengers and mdeaths in column "var"


context("irregular conversion handling")

####### Monthly series: AirPassengers #####
test_that("conversion produces right classes", {
  expect_s3_class(ts_xts(window(AirPassengers, start = c(1949, 5))), "xts")
  expect_s3_class(ts_ts(window(AirPassengers, start = c(1949, 5))), "ts")
  expect_s3_class(ts_df(window(AirPassengers, start = c(1949, 5))), "data.frame")
  expect_s3_class(ts_dt(window(AirPassengers, start = c(1949, 5))), "data.table")
  expect_s3_class(ts_tbl(window(AirPassengers, start = c(1949, 5))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_xts(window(AirPassengers, start = c(1949, 5)))), "xts")
  expect_s3_class(ts_ts(ts_xts(window(AirPassengers, start = c(1949, 5)))), "ts")
  expect_s3_class(ts_df(ts_xts(window(AirPassengers, start = c(1949, 5)))), "data.frame")
  expect_s3_class(ts_dt(ts_xts(window(AirPassengers, start = c(1949, 5)))), "data.table")
  expect_s3_class(ts_tbl(ts_xts(window(AirPassengers, start = c(1949, 5)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_df(window(AirPassengers, start = c(1949, 5)))), "xts")
  expect_s3_class(ts_ts(ts_df(window(AirPassengers, start = c(1949, 5)))), "ts")
  expect_s3_class(ts_df(ts_df(window(AirPassengers, start = c(1949, 5)))), "data.frame")
  expect_s3_class(ts_dt(ts_df(window(AirPassengers, start = c(1949, 5)))), "data.table")
  expect_s3_class(ts_tbl(ts_df(window(AirPassengers, start = c(1949, 5)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_dt(window(AirPassengers, start = c(1949, 5)))), "xts")
  expect_s3_class(ts_ts(ts_dt(window(AirPassengers, start = c(1949, 5)))), "ts")
  expect_s3_class(ts_df(ts_dt(window(AirPassengers, start = c(1949, 5)))), "data.frame")
  expect_s3_class(ts_dt(ts_dt(window(AirPassengers, start = c(1949, 5)))), "data.table")
  expect_s3_class(ts_tbl(ts_dt(window(AirPassengers, start = c(1949, 5)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_tbl(window(AirPassengers, start = c(1949, 5)))), "xts")
  expect_s3_class(ts_ts(ts_tbl(window(AirPassengers, start = c(1949, 5)))), "ts")
  expect_s3_class(ts_df(ts_tbl(window(AirPassengers, start = c(1949, 5)))), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(window(AirPassengers, start = c(1949, 5)))), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(window(AirPassengers, start = c(1949, 5)))), "tbl_df")
  
})

test_that("conversion between objects works as expected: AirPassengers", {
  
  x.ts <- window(AirPassengers, start = c(1949, 5))
  
  x.xts <- ts_xts(x.ts, cname = 'AirPassengers')
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)
  
  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)
  
  expect_equal(ts_xts(ts_ts(x.xts), cname = "AirPassengers"), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)
  
  expect_equal(ts_df(ts_ts(x.df), cname = "AirPassengers"), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)
  
  expect_equal(ts_dt(ts_ts(x.dt), cname = "AirPassengers"), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)
  
  expect_equal(ts_tbl(ts_ts(x.tbl), cname = "AirPassengers"), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})

####### Regular series: EuStockMarkets #####
test_that("conversion produces right classes", {
  expect_s3_class(ts_xts(window(EuStockMarkets, start = c(1991, 200))), "xts")
  expect_s3_class(ts_ts(window(EuStockMarkets, start = c(1991, 200))), "ts")
  expect_s3_class(ts_df(window(EuStockMarkets, start = c(1991, 200))), "data.frame")
  expect_s3_class(ts_dt(window(EuStockMarkets, start = c(1991, 200))), "data.table")
  expect_s3_class(ts_tbl(window(EuStockMarkets, start = c(1991, 200))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_xts(window(EuStockMarkets, start = c(1991, 200)))), "xts")
  expect_s3_class(ts_ts(ts_xts(window(EuStockMarkets, start = c(1991, 200)))), "ts")
  expect_s3_class(ts_df(ts_xts(window(EuStockMarkets, start = c(1991, 200)))), "data.frame")
  expect_s3_class(ts_dt(ts_xts(window(EuStockMarkets, start = c(1991, 200)))), "data.table")
  expect_s3_class(ts_tbl(ts_xts(window(EuStockMarkets, start = c(1991, 200)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_df(window(EuStockMarkets, start = c(1991, 200)))), "xts")
  expect_s3_class(ts_ts(ts_df(window(EuStockMarkets, start = c(1991, 200)))), "ts")
  expect_s3_class(ts_df(ts_df(window(EuStockMarkets, start = c(1991, 200)))), "data.frame")
  expect_s3_class(ts_dt(ts_df(window(EuStockMarkets, start = c(1991, 200)))), "data.table")
  expect_s3_class(ts_tbl(ts_df(window(EuStockMarkets, start = c(1991, 200)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_dt(window(EuStockMarkets, start = c(1991, 200)))), "xts")
  expect_s3_class(ts_ts(ts_dt(window(EuStockMarkets, start = c(1991, 200)))), "ts")
  expect_s3_class(ts_df(ts_dt(window(EuStockMarkets, start = c(1991, 200)))), "data.frame")
  expect_s3_class(ts_dt(ts_dt(window(EuStockMarkets, start = c(1991, 200)))), "data.table")
  expect_s3_class(ts_tbl(ts_dt(window(EuStockMarkets, start = c(1991, 200)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_tbl(window(EuStockMarkets, start = c(1991, 200)))), "xts")
  expect_s3_class(ts_ts(ts_tbl(window(EuStockMarkets, start = c(1991, 200)))), "ts")
  expect_s3_class(ts_df(ts_tbl(window(EuStockMarkets, start = c(1991, 200)))), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(window(EuStockMarkets, start = c(1991, 200)))), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(window(EuStockMarkets, start = c(1991, 200)))), "tbl_df")
  
})

test_that("conversion between objects works as expected: EuStockMarkets ", {
  
  x.ts <- window(EuStockMarkets, start = c(1991, 200))
  
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

####### Quarterly series: austres #####
test_that("conversion produces right classes", {
  expect_s3_class(ts_xts(window(austres, start = c(1975, 3))), "xts")
  expect_s3_class(ts_ts(window(austres, start = c(1975, 3))), "ts")
  expect_s3_class(ts_df(window(austres, start = c(1975, 3))), "data.frame")
  expect_s3_class(ts_dt(window(austres, start = c(1975, 3))), "data.table")
  expect_s3_class(ts_tbl(window(austres, start = c(1975, 3))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_xts(window(austres, start = c(1975, 3)))), "xts")
  expect_s3_class(ts_ts(ts_xts(window(austres, start = c(1975, 3)))), "ts")
  expect_s3_class(ts_df(ts_xts(window(austres, start = c(1975, 3)))), "data.frame")
  expect_s3_class(ts_dt(ts_xts(window(austres, start = c(1975, 3)))), "data.table")
  expect_s3_class(ts_tbl(ts_xts(window(austres, start = c(1975, 3)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_df(window(austres, start = c(1975, 3)))), "xts")
  expect_s3_class(ts_ts(ts_df(window(austres, start = c(1975, 3)))), "ts")
  expect_s3_class(ts_df(ts_df(window(austres, start = c(1975, 3)))), "data.frame")
  expect_s3_class(ts_dt(ts_df(window(austres, start = c(1975, 3)))), "data.table")
  expect_s3_class(ts_tbl(ts_df(window(austres, start = c(1975, 3)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_dt(window(austres, start = c(1975, 3)))), "xts")
  expect_s3_class(ts_ts(ts_dt(window(austres, start = c(1975, 3)))), "ts")
  expect_s3_class(ts_df(ts_dt(window(austres, start = c(1975, 3)))), "data.frame")
  expect_s3_class(ts_dt(ts_dt(window(austres, start = c(1975, 3)))), "data.table")
  expect_s3_class(ts_tbl(ts_dt(window(austres, start = c(1975, 3)))), "tbl_df")
  
  expect_s3_class(ts_xts(ts_tbl(window(austres, start = c(1975, 3)))), "xts")
  expect_s3_class(ts_ts(ts_tbl(window(austres, start = c(1975, 3)))), "ts")
  expect_s3_class(ts_df(ts_tbl(window(austres, start = c(1975, 3)))), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(window(austres, start = c(1975, 3)))), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(window(austres, start = c(1975, 3)))), "tbl_df")
  
})

test_that("conversion between objects works as expected: austres", {
  
  x.ts <- window(austres, start = c(1975, 3))
  
  x.xts <- ts_xts(x.ts, cname = 'austres')
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)
  
  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)
  
  expect_equal(ts_xts(ts_ts(x.xts), cname = "austres"), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)
  
  expect_equal(ts_df(ts_ts(x.df), cname = "austres"), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)
  
  expect_equal(ts_dt(ts_ts(x.dt), cname = "austres"), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)
  
  expect_equal(ts_tbl(ts_ts(x.tbl), cname = "austres"), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})

####### Irregular series: #####
test_that("conversion produces right classes", {
  
  t <- window(AirPassengers, start = c(1951,3) )
  window(t, deltat = 1) <- NA
  
  expect_s3_class(ts_xts(t), "xts")
  expect_s3_class(ts_ts(t), "ts")
  expect_s3_class(ts_df(t), "data.frame")
  expect_s3_class(ts_dt(t), "data.table")
  expect_s3_class(ts_tbl(t), "tbl_df")
  
  expect_s3_class(ts_xts(ts_xts(t)), "xts")
  expect_s3_class(ts_ts(ts_xts(t)), "ts")
  expect_s3_class(ts_df(ts_xts(t)), "data.frame")
  expect_s3_class(ts_dt(ts_xts(t)), "data.table")
  expect_s3_class(ts_tbl(ts_xts(t)), "tbl_df")
  
  expect_s3_class(ts_xts(ts_df(t)), "xts")
  expect_s3_class(ts_ts(ts_df(t)), "ts")
  expect_s3_class(ts_df(ts_df(t)), "data.frame")
  expect_s3_class(ts_dt(ts_df(t)), "data.table")
  expect_s3_class(ts_tbl(ts_df(t)), "tbl_df")
  
  expect_s3_class(ts_xts(ts_dt(t)), "xts")
  expect_s3_class(ts_ts(ts_dt(t)), "ts")
  expect_s3_class(ts_df(ts_dt(t)), "data.frame")
  expect_s3_class(ts_dt(ts_dt(t)), "data.table")
  expect_s3_class(ts_tbl(ts_dt(t)), "tbl_df")
  
  expect_s3_class(ts_xts(ts_tbl(t)), "xts")
  expect_s3_class(ts_ts(ts_tbl(t)), "ts")
  expect_s3_class(ts_df(ts_tbl(t)), "data.frame")
  expect_s3_class(ts_dt(ts_tbl(t)), "data.table")
  expect_s3_class(ts_tbl(ts_tbl(t)), "tbl_df")
  
})

test_that("conversion between objects works as expected", {
  
  x.ts  <- window(AirPassengers, start = c(1951,3) )
  window(x.ts , deltat = 1) <- NA

  x.xts <- ts_xts(x.ts, cname = 'AirPassengers')
  x.df <- ts_df(x.xts)
  x.dt <- ts_dt(x.df)
  x.tbl <- ts_tbl(x.dt)
  
  expect_equal(ts_ts(ts_xts(x.ts)), x.ts)
  expect_equal(ts_ts(ts_df(x.ts)), x.ts)
  expect_equal(ts_ts(ts_dt(x.ts)), x.ts)
  expect_equal(ts_ts(ts_tbl(x.ts)), x.ts)
  
  expect_equal(ts_xts(ts_ts(x.xts), cname = "AirPassengers"), x.xts)
  expect_equal(ts_xts(ts_df(x.xts)), x.xts)
  expect_equal(ts_xts(ts_dt(x.xts)), x.xts)
  expect_equal(ts_xts(ts_tbl(x.xts)), x.xts)
  
  expect_equal(ts_df(ts_ts(x.df), cname = "AirPassengers"), x.df)
  expect_equal(ts_df(ts_xts(x.df)), x.df)
  expect_equal(ts_df(ts_dt(x.df)), x.df)
  expect_equal(ts_df(ts_tbl(x.df)), x.df)
  
  expect_equal(ts_dt(ts_ts(x.dt), cname = "AirPassengers"), x.dt)
  expect_equal(ts_dt(ts_xts(x.dt)), x.dt)
  expect_equal(ts_dt(ts_df(x.dt)), x.dt)
  expect_equal(ts_dt(ts_tbl(x.dt)), x.dt)
  
  expect_equal(ts_tbl(ts_ts(x.tbl), cname = "AirPassengers"), x.tbl)
  expect_equal(ts_tbl(ts_xts(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_df(x.tbl)), x.tbl)
  expect_equal(ts_tbl(ts_dt(x.tbl)), x.tbl)
})


