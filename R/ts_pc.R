#' First Differences and Percentage Change Rates
#'
#' `ts_pcy` and `ts_diffy` calculate the percentage change rate and the difference
#' compared to the previous period, `ts_pcy` and `ts_diffy` calculate compared to
#' the same period of the previous year.
#'
#' @inherit ts_dts
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' tail(ts_diff(ts_c(fdeaths, mdeaths)))
#' tail(ts_pc(ts_c(fdeaths, mdeaths)))
#' tail(ts_pcy(ts_c(fdeaths, mdeaths)))
#' tail(ts_diffy(ts_c(fdeaths, mdeaths)))
#' @export
ts_pc <- function(x) {
  ts_apply(x, function(x) {
    x[, .(time, value = 100 * (value / c(NA, value[-length(value)]) - 1))]
  })
}


#' @name ts_pc
#' @export
ts_diff <- function(x) {
  ts_apply(x, function(x) {
    x[, .(time, value = value - c(NA, value[-length(value)]))]
  })
}


#' @name ts_pc
#' @export
ts_pcy <- function(x) {
  ts_apply(x, function(x) {
    xlag <- data.table(time = time_shift(x$time, "1 year"), lag = x$value)
    xlag[x, on = "time"][, .(time, value = (value / lag - 1) * 100)]
  })
}


#' @name ts_pc
#' @export
ts_diffy <- function(x) {
  ts_apply(x, function(x) {
    xlag <- data.table(time = time_shift(x$time, "1 year"), lag = x$value)
    xlag[x, on = "time"][, .(time, value = value - lag)]
  })
}



# #' @name ts_pc
# #' @export
# ts_pca <- function(x) {
#   stopifnot(ts_boxable(x))
#   x.dts <- ts_pc(copy(ts_dts(x)))
#   # annualization
#   cname <- dts_cname(x.dts)
#   setnames(x.dts, cname$time, "time")
#   setnames(x.dts, cname$value, "value")
#   .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
#   x.dts[, value := 100 * ((1 + (value / 100))^frequency_one(time, "decimal") - 1), by = eval(.by)]$string
#   setnames(x.dts, "time", cname$time)
#   setnames(x.dts, "value", cname$value)
#   copy_class(x.dts, x)
# }
