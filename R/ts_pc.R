#' First Differences and Percentage Change Rates
#'
#' `ts_pcy` and `ts_diffy` calculate the percentage change rate and the
#' difference compared to the previous period, `ts_pcy` and `ts_diffy` calculate
#' the percentage change rate compared to the same period of the previous year.
#' `ts_pca` calculates annualized percentage change rates compared to the
#' previous period.
#'
#' @inherit ts_default
#' @examples
#' ts_diff(ts_c(fdeaths, mdeaths))
#' ts_pc(ts_c(fdeaths, mdeaths))
#' ts_pca(ts_c(fdeaths, mdeaths))
#' ts_pcy(ts_c(fdeaths, mdeaths))
#' ts_diffy(ts_c(fdeaths, mdeaths))
#' @export
ts_pc <- function(x) {
  ts_apply(ts_regular(x), function(x) {
    value <- NULL
    x[, list(time, value = 100 * (as.numeric(value) / c(NA_real_, as.numeric(value)[-length(value)]) - 1))]
  })
}


#' @name ts_pc
#' @export
ts_diff <- function(x) {
  ts_apply(ts_regular(x), function(x) {
    value <- NULL
    x[, list(time, value = as.numeric(value) - c(NA_real_, as.numeric(value)[-length(value)]))]
  })
}


#' @name ts_pc
#' @export
ts_pca <- function(x) {
  ts_apply(ts_regular(x), function(x) {
    fr <- frequency_one(x$time)$freq
    value <- NULL
    x[
      ,
      list(time, value = 100 * ((as.numeric(value) / c(NA_real_, as.numeric(value)[-length(value)]))^fr - 1))
    ]
  })
}


#' @name ts_pc
#' @export
ts_pcy <- function(x) {
  ts_apply(ts_regular(x), function(x) {
    value <- NULL
    value_lag <- NULL
    xlag <- data.table(time = time_shift(x$time, "1 year"), value_lag = x$value)
    xlag[x, on = "time"][, list(time, value = (value / value_lag - 1) * 100)]
  })
}

#' @name ts_pc
#' @export
ts_diffy <- function(x) {
  ts_apply(ts_regular(x), function(x) {
    value <- NULL
    value_lag <- NULL
    xlag <- data.table(time = time_shift(x$time, "1 year"), value_lag = x$value)
    xlag[x, on = "time"][, list(time, value = value - value_lag)]
  })
}
