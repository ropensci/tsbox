library(dplyr)


#' @srrstats {G5.8} **Edge condition tests** *to test that these conditions produce expected behaviour such as clear warnings or errors when confronted with data with extreme properties including but not limited to:*
#' @srrstats {G5.8a} *Zero-length data*
test_that("Zero-length data", {
  expect_error(ts_trend(mdeaths[1 == 2]), "non-ts-boxable")
  expect_message(ts_trend(filter(ts_tbl(mdeaths), 1 == 2)), "Return input series")

  x <- filter(ts_tbl(mdeaths), 1 == 2)

  expect_error(ts_first_of_period(x), "at least two observations")
  expect_error(ts_na_interpolation(x), "at least two observations")
  expect_error(ts_forecast(x), "at least two observations")
  expect_error(ts_diff(x), "at least two observations")
  expect_error(ts_diffy(x), "at least two observations")
  expect_error(ts_pc(x), "at least two observations")
  expect_error(ts_pca(x), "at least two observations")
  expect_error(ts_lag(x), "at least two observations")

  expect_s3_class(ts_index(x), "tbl_df")
  expect_s3_class(ts_regular(x), "tbl_df")
  expect_s3_class(ts_span(x), "tbl_df")

})


#' @srrstats {G5.8b} *Data of unsupported types (e.g., character or complex numbers in for functions designed only for numeric data)*
test_that("Unsupported types: char, complex", {

  # It may be possilbe to allwo for these types, but it will add quite a bit
  # of complexity. Not sure if there is a use case.

  x <- ts_tbl(mdeaths)

  x$value <- "aaa"
  expect_error(ts_ts(x), "not numeric")

  x$value <- 0i
  expect_error(ts_ts(x), "not numeric")

})



#' @srrstats {G5.8c} *Data with all-`NA` fields or columns or all identical fields or columns*
test_that("all-`NA` fields are fine", {

  x <- ts_tbl(ts_c(mdeaths))
  x$value <- NA_real_

  expect_error(ts_first_of_period(x), "at least two observations")
  # expect_error(ts_na_interpolation(x), "at least two observations")
  expect_error(ts_forecast(x), "at least two observations")

  expect_s3_class(ts_diff(x), "tbl_df")
  expect_s3_class(ts_diffy(x), "tbl_df")
  expect_s3_class(ts_pc(x), "tbl_df")
  expect_s3_class(ts_pca(x), "tbl_df")
  expect_s3_class(ts_lag(x), "tbl_df")

  expect_s3_class(ts_index(x), "tbl_df")
  expect_s3_class(ts_regular(x), "tbl_df")
  expect_s3_class(ts_span(x), "tbl_df")

})


#' @srrstats {G5.8d} *Data outside the scope of the algorithm (for example, data with more fields (columns) than observations (rows) for some regression algorithms)*
test_that("Data outside the scope of the algorithm", {

  x <- ts_tbl(mdeaths)

  # additional character or factors are treated as additional id cols, and do
  # not affect results
  x$one_more <- "sdfsd"
  fl <- lst(
    ts_bind,
    ts_c,
    ts_chain,
    ts_default,
    ts_diff,
    ts_diffy,
    ts_first_of_period,
    # ts_forecast,
    ts_index,
    ts_lag,
    ts_na_interpolation,
    ts_pc,
    ts_pca,
    ts_regular,
    # ts_seas,
    ts_span
  )

  for (i in seq(fl)){
    message(names(fl)[i])
    z <- fl[[i]](x)
    expect_s3_class(z, "tbl_df")
  }

  x$one_more <- as.factor("sdfsd")
  for (i in seq(fl)){
    message(names(fl)[i])
    z <- fl[[i]](x)
    expect_s3_class(z, "tbl_df")
  }

  # numeric also works with canonical col names: one_more becomes id col
  x$one_more <- 1
  for (i in seq(fl)){
    message(names(fl)[i])
    z <- fl[[i]](x)
    expect_s3_class(z, "tbl_df")
  }
})


test_that("Processing of badly shaped data works as expected", {
  # without with canonical col names, the new column is detected as value column
  # value2 becomes id column, which results in many lenght 1 time series.

  x <- ts_tbl(mdeaths)
  x <- rename(x, value2 = value)
  x$one_more <- 1

  expect_message(
    z <- ts_dts(x) ,
   "Are you using a wide data frame?"
  )

  expect_error(ts_first_of_period(x), "at least two observations")
  # expect_warning(ts_na_interpolation(x), "imputeTS: No imputation performed")
  expect_error(ts_forecast(x), "at least two observations")
  expect_error(ts_diff(x), "at least two observations")
  expect_error(ts_diffy(x), "at least two observations")
  expect_error(ts_pc(x), "at least two observations")
  expect_error(ts_pca(x), "at least two observations")
  expect_error(ts_lag(x), "at least two observations")

  expect_s3_class(ts_bind(x), "tbl_df")
  expect_s3_class(ts_c(x), "tbl_df")
  expect_s3_class(ts_chain(x), "tbl_df")
  expect_s3_class(ts_default(x), "tbl_df")
  expect_s3_class(ts_index(x), "tbl_df")
  expect_s3_class(ts_regular(x), "tbl_df")
  expect_s3_class(ts_span(x), "tbl_df")

})



