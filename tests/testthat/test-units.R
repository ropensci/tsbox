library(dplyr)
library(units)

#' @srrstats {G2.11} *Software should ensure that `data.frame`-like tabular
#'  objects which have columns which do not themselves have standard class
#'  attributes (typically, `vector`) are appropriately processed, and do not
#'  error without reason. This behaviour should be tested. Again, columns
#'  created by the `units` package provide a good test case.*
test_that("tsbox works with units", {

  x <-
    ts_tbl(ts_c(mdeaths, fdeaths)) %>%
    mutate(value = set_units(value, m))

  z <- ts_pick(x, "fdeaths")
  expect_identical(units(z$value)$numerator, "m")

  # functions that should work with units
  fl <- lst(
    ts_bind,
    ts_c,
    ts_chain,
    ts_default,
    ts_diff,
    ts_diffy,
    ts_first_of_period,
    ts_forecast,
    ts_index,
    ts_lag,
    ts_na_interpolation,
    ts_pc,
    ts_pca,
    ts_regular,
    ts_seas,
    ts_span
  )

  for (i in seq(fl)){
    message(names(fl)[i])
    z <- fl[[i]](x)
    expect_identical(units(z$value)$numerator, "m", label = names(fl)[i])
  }

})
