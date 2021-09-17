test_that("integer and character shifting works the same", {
  expect_equal(ts_lag(mdeaths), ts_lag(mdeaths, "month"))
  expect_equal(ts_lag(austres), ts_lag(austres, "quarter"))

  expect_equal(ts_lag(mdeaths, 5), ts_lag(mdeaths, "5 month"))
  expect_equal(ts_lag(austres, -3), ts_lag(austres, "-3 quarter"))

  expect_equal(ts_lag(discoveries, -300), ts_lag(discoveries, "-300 years"))
  expect_equal(ts_lag(fdeaths, 11), ts_lag(fdeaths, "11 month"))
})


#' @srrstats {G5.4b} *For new implementations of existing methods, correctness tests should include tests against previous implementations. Such testing may explicitly call those implementations in testing, preferably from fixed-versions of other software, or use stored outputs from those where that is not possible.*
#'   Compaare ts_lag with stats::lag
test_that("ts_lag works as stats::lag", {
  expect_equal(ts_lag(mdeaths), stats::lag(mdeaths, -1))
  expect_equal(ts_lag(mdeaths, -1), stats::lag(mdeaths, 1))

  expect_equal(ts_lag(mdeaths, 12), stats::lag(mdeaths, -12))
  expect_equal(ts_lag(mdeaths, -12), stats::lag(mdeaths, 12))
})

test_that("ts_lag works both ways", {
  expect_equal(ts_lag(ts_lag(mdeaths, -1)), ts_lag(ts_lag(mdeaths), -1))
  expect_equal(ts_lag(ts_lag(mdeaths, -12)), ts_lag(ts_lag(mdeaths), -12))
})




