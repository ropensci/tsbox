#' @srrstats {G5.3} *For functions which are expected to return objects containing no missing (`NA`) or undefined (`NaN`, `Inf`) values, the absence of any such values in return objects should be explicitly tested.*
test_that("functions which are expected to return objects containing no missing values do so.", {

  x <- fdeaths
  x[5] <- NA_real_
  x <- ts_tbl(x)
  expect_true(any(is.na(x$value)))

  z <- ts_na_omit(x)
  expect_false(any(is.na(z$value)))

  z <- ts_na_interpolation(x)
  expect_false(any(is.na(z$value)))
})


