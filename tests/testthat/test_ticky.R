library(testthat)
library(data.table) 
library(tsbox)
library(dplyr)
library(tsbox)




# Major Things

# - [ ] ts_rbind, rbind, binding POSIXct and Data should result in POSIXct
# - [ ] unified handling of deparse(substitute(x))

# - [ ] reclassing of numeric, matrix output, similar to xts::reclass



    #' @export
    #' @rdname ts_
    # ts_scale <- ts_(function(x, ...){
    #   z <- scale.default(unclass(x), ...)
    #   xts::reclass(z, x)
    # }, class = "xts")



context("tricky stuff")


# ts_bind(EuStockMarkets, mdeaths, fdeaths)

# Error in rbindlist(ll.dts) : 
#   Class attributes at column 1 of input list at position 2 does not match with column 1 of input list at position 1. Coercion of objects of class 'factor' alone is handled internally by rbind/rbindlist at the moment.










test_that("ts_gather and ts_spread work both ways.", {
  a <- ts_df(ts_bind(AirPassengers, mdeaths, fdeaths))
  expect_equal(a, ts_gather(ts_spread(a)))

  b <- ts_tbl(ts_dt(EuStockMarkets))
  expect_equal(b, ts_gather(ts_spread(b)))
})




