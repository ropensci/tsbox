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



# error w short series

test_that("ts_rbind works as it should.", {
  expect_equal(AirPassengers, ts_rbind(ts_window(AirPassengers, start = "1950-01-01"), ts_window(AirPassengers, end = "1949-12-01")))
  expect_equal(ts_dt(AirPassengers), ts_rbind(AirPassengers = ts_window(ts_dt(AirPassengers), start = "1950-01-01"), ts_window(ts_dt(AirPassengers), end = "1949-12-01")))
  expect_equal(ts_df(AirPassengers), ts_rbind(AirPassengers = ts_window(ts_df(AirPassengers), start = "1950-01-01"), ts_window(ts_df(AirPassengers), end = "1949-12-01")))
  expect_equal(ts_tbl(AirPassengers), ts_rbind(AirPassengers = ts_window(ts_tbl(AirPassengers), start = "1950-01-01"), ts_window(ts_tbl(AirPassengers), end = "1949-12-01")))
})


test_that("Latest tricky stuff works.", {



  expect_equal(mdeaths, ts_ts(ts_select(ts_c(mdeaths, austres, AirPassengers, DAX = EuStockMarkets[,'DAX']), 'mdeaths')))

  # names must be unique!!
  a <- ts_dts(ts_c(AirPassengers, AirPassengers))
  expect_true(length(unique(a[['var']])) == 2)


})





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





test_that("Some trickier stuff works.", {

  expect_s3_class(ts_c(EuStockMarkets, mdeaths, fdeaths), "data.frame")

  x <- ts_c(ts_df(ts_c(mdeaths, fdeaths)), AirPassengers)
  expect_equal(ts_ts(ts_select(x, "AirPassengers")), AirPassengers)

  # series of length 2
  a <- ts_dts(window(AirPassengers, end = c(1949, 2)))
  ts_ts(a)
})



test_that("No Invalid .internal.selfref detected.", {
  x <- ts_dts(AirPassengers)
  expect_silent(x[, s := "sdfsd"])
})


test_that("ts_gather and ts_spread work both ways.", {
  a <- ts_df(ts_c(ts_dt(AirPassengers), mdeaths, fdeaths))
  expect_equal(a, ts_na_omit(ts_gather(ts_spread(a))))

  b <- ts_tbl(ts_dt(EuStockMarkets))
  expect_equal(b, ts_gather(ts_spread(b)))
})




