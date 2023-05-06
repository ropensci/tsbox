library(dplyr)

#' @srrstats {G2.15} *Functions should never assume non-missingness, and should
#'  never pass data with potential missing values to any base routines with
#'  default `na.rm = FALSE`-type parameters (such as
#'  [`mean()`](https://stat.ethz.ch/R-manual/R-devel/library/base/html/mean.html),
#'  [`sd()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/sd.html) or
#'  [`cor()`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/cor.html)).*

test_that("Functions work with missing values", {

  x <- fdeaths
  x[5] <- NA_real_

  # functions that keep NA
  fl <- lst(
    ts_bind,
    ts_c,
    ts_chain,
    ts_default,
    ts_diff,
    ts_diffy,
    ts_first_of_period,
    # ts_forecast,         #
    ts_index,
    ts_lag,
    # ts_na_interpolation,
    ts_pc,
    ts_pca,
    ts_regular,
    # ts_seas,
    ts_span
  )

  for (i in seq(fl)){
    message(names(fl)[i])
    z <- fl[[i]](x)
    expect_identical(z[5], NA_real_)
  }

  # functions that remvoe NA
  expect_false(is.na(ts_na_interpolation(x)[5]))
  # expect_false(is.na(ts_seas(x, na.action = seasonal::na.x13)[5]))

})


#' @srrstats {G2.16} *All functions should also provide options to handle
#'  undefined values (e.g., `NaN`, `Inf` and `-Inf`), including potentially
#'  ignoring or removing such values.*
test_that("Functions keep NaN values", {

  x <- fdeaths
  x[5] <- NaN

  # functions that keep NA
  fl <- lst(
    ts_bind,
    ts_c,
    ts_chain,
    ts_default,
    ts_diff,
    # ts_diffy,
    ts_first_of_period,
    # ts_forecast,         #
    ts_index,
    ts_lag,
    # ts_na_interpolation,
    ts_pc,
    ts_pca,
    ts_regular,
    # ts_seas,
    ts_span
  )

  for (i in seq(fl)){
    message(names(fl)[i])
    z <- fl[[i]](x)
    expect_identical(z[5], NaN)
  }

  expect_identical(ts_diffy(x)[5 + 12], NaN)

  # functions that remvoe NA
  expect_false(is.na(ts_na_interpolation(x)[5]))

})






#' @srrstats {TS2.0} *Time Series Software which presumes or requires
#'  regular data should only allow **explicit** missing values, and should
#'  issue appropriate diagnostic messages, potentially including errors, in
#'  response to any **implicit** missing values.*
test_that("Implicit and explicit NAs are treated the same", {

  x <- fdeaths
  x[5] <- NA_real_
  explicit <- ts_tbl(x)
  implicit <- ts_na_omit(explicit)

  expect_equal(ts_ts(ts_trend(explicit)), ts_ts(ts_trend(implicit)))
  expect_equal(ts_ts(ts_bind(explicit)), ts_ts(ts_bind(implicit)))
  expect_equal(ts_ts(ts_chain(explicit)), ts_ts(ts_chain(implicit)))
  expect_equal(ts_ts(ts_diff(explicit)), ts_ts(ts_diff(implicit)))
  expect_equal(ts_ts(ts_diffy(explicit)), ts_ts(ts_diffy(implicit)))
  expect_equal(ts_ts(ts_first_of_period(explicit)), ts_ts(ts_first_of_period(implicit)))
  expect_equal(ts_ts(ts_index(explicit)), ts_ts(ts_index(implicit)))
  expect_equal(ts_ts(ts_lag(explicit)), ts_ts(ts_lag(implicit)))
  expect_equal(ts_ts(ts_pc(explicit)), ts_ts(ts_pc(implicit)))
  expect_equal(ts_ts(ts_pca(explicit)), ts_ts(ts_pca(implicit)))
  expect_equal(ts_ts(ts_span(explicit)), ts_ts(ts_span(implicit)))

})
