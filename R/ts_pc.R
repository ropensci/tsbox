#' Lags, Differences, Percentage Change Rates
#'
#' @param x a ts_boxable object
#' @param lag defined as in dplyr, opposite to R base
#' @param fill how to fill missing values
#' @param ... additional arguments, passed to subfunctions
#' @examples
#' ts_lag(ts_c(fdeaths, mdeaths))
#' ts_diff(ts_c(fdeaths, mdeaths))
#' ts_pc(ts_c(fdeaths, mdeaths))
#' @export
ts_lag <- function(x, lag = 1, fill = NA) {
  value <- NULL

  z <- ts_dts(x)

  if (lag < 0) {
    type <- "lead"
    lag <- -lag
  } else {
    type <- "lag"
  }

  colname.id <- colname_id(z)
  .by <- parse(text = paste0("list(", paste(colname.id, collapse = ", "), ")"))

  # do not use ts_apply here, to take advantage of data.table speed
  z[
    ,
    value := shift(value, n = lag, fill = fill, type = type, give.names = FALSE),
    by = eval(.by)
  ]

  ts_na_omit(ts_reclass(z, x))
}




# This probably could make use of data.table::shift

pc_core <- function(x) {
  100 * ((x / stats::lag(x, -1)) - 1)
}
pcy_core <- function(x) {
  100 * ((x / stats::lag(x, -frequency(x))) - 1)
}


#' @name ts_lag
#' @export
ts_pc <- ts_(pc_core, vectorize = TRUE)

#' @name ts_lag
#' @export
ts_pcy <- ts_(pcy_core, vectorize = TRUE)

#' @name ts_lag
#' @export
ts_diff <- ts_(diff, vectorize = TRUE)

#' @name ts_lag
#' @export
ts_diffy <- ts_(function(x) diff(x, lag = frequency(x)), vectorize = TRUE)
