library(testthat)
library(tsbox)




# Major Things

# - [ ] ts_rbind, ts_c, binding POSIXct and Data should result in POSIXct, 
#       also should have the same var naming behavior)
# perhaps this can be done by letting ts_rbind calling ts_c

# - [ ] unified handling of deparse(substitute(x))

# - heuristic = TURE to all conversion functions?
# - var.names = NULL
# var.names <- detect_var_names(var.names, deparse(substitute(x)))


# - [ ] reclassing of numeric, matrix output, similar to xts::reclass

    #' @export
    #' @rdname ts_
    # ts_scale <- ts_(function(x, ...){
    #   z <- scale.default(unclass(x), ...)
    #   xts::reclass(z, x)
    # }, class = "xts")

# - [ ] a cleaner ts_ function


# - [ ] Lookup table for heuristic frequency conversion



# - [ ] Tools to enforce regularity (major and quite separate task)


context("tricky stuff")


expect_s3_class(ts_c(EuStockMarkets, mdeaths, fdeaths), "data.table")

x <- ts_c(ts_df(ts_c(mdeaths, fdeaths)), AirPassengers)
expect_equal(ts_ts(ts_select(x, "AirPassengers")), AirPassengers)



# Error in rbindlist(ll.dts) : 
#   Class attributes at column 1 of input list at position 2 does not match with column 1 of input list at position 1. Coercion of objects of class 'factor' alone is handled internally by rbind/rbindlist at the moment.










test_that("ts_gather and ts_spread work both ways.", {
  a <- ts_df(ts_c(AirPassengers, mdeaths, fdeaths))
  expect_equal(a, ts_gather(ts_spread(a)))

  b <- ts_tbl(ts_dt(EuStockMarkets))
  expect_equal(b, ts_gather(ts_spread(b)))
})




