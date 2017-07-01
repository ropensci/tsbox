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


# - [ ] numeric vectors in ts_window()


# - [ ] Tools to enforce regularity (major and quite separate task)


context("tricky stuff")


expect_s3_class(ts_c(EuStockMarkets, mdeaths, fdeaths), "data.table")

x <- ts_c(ts_df(ts_c(mdeaths, fdeaths)), AirPassengers)
expect_equal(ts_ts(ts_select(x, "AirPassengers")), AirPassengers)

# Error in rbindlist(ll.dts) : 
#   Class attributes at column 1 of input list at position 2 does not match with column 1 of input list at position 1. Coercion of objects of class 'factor' alone is handled internally by rbind/rbindlist at the moment.



# error w short series


# that works
a <- ts_dts(window(AirPassengers, end = c(1949, 2)))
ts_ts(a)

a <- ts_dts(window(AirPassengers, end = c(1949, 1)))

# that should give a decent error
ts_ts(a)  






# more careful is_time checking (and test)

# Here, it sees arten as a year, but it shouldn't for at least 2 reasons:

# - Year >= 3000 should not be supported
# - Variable is not continous (but this is probably a bad reason)

# -> Years should look like a year: 

# > dta
#         arten funk jahr       value
#      1:  1000   -1 1990  17276468.3
#      2:  1001   -1 1990 262836087.5
#      3:  1002   -1 1990 352935857.3
#      4:  1010   -1 1990 945041263.6
#      5:  1011   -1 1990 676671322.9
#     ---                            
# 101851:  4892  990 2015   2558990.6
# 101852:  4894  990 2015   3271095.1
# 101853:  4895  990 2015  87363643.8
# 101854:  4899  990 2015   2000000.0
# 101855:  4390  995 2015    711948.7

# ts_ts(ts_dts(dta[arten == "4489"]))





test_that("No Invalid .internal.selfref detected.", {
  x <- ts_dts(AirPassengers)
  expect_silent(x[, s := "sdfsd"])
})


test_that("ts_gather and ts_spread work both ways.", {
  a <- ts_df(ts_c(AirPassengers, mdeaths, fdeaths))
  expect_equal(a, ts_gather(ts_spread(a)))

  b <- ts_tbl(ts_dt(EuStockMarkets))
  expect_equal(b, ts_gather(ts_spread(b)))
})




