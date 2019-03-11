#' First Differences and Percentage Change Rates
#'
#' `ts_pcy` and `ts_diffy` calculate the percentage change rate and the difference
#' compared to the previous period, `ts_pcy` and `ts_diffy` calculate compared to
#' the same period of the previous year.
#'
#' @inherit ts_dts
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' head(ts_diff(ts_c(fdeaths, mdeaths)))
#' head(ts_pc(ts_c(fdeaths, mdeaths)))
#' head(ts_pcy(ts_c(fdeaths, mdeaths)))
#' head(ts_diffy(ts_c(fdeaths, mdeaths)))
#' @export
ts_pc <- function(x) {
  ts_apply_time_value(x, function(time, value) {
    100 * ((value / c(NA, value[-length(value)])) - 1)
  })
}

ts_pc_old <- function(x) {
  stopifnot(ts_boxable(x))
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(((z %ts/% ts_lag(z)) %ts-% 1) %ts*% 100, x)
}

ts_apply_time_value <- function(x, fun, regular = TRUE) {
  value <- NULL
  if (regular) {
    x.dts <- ts_dts(ts_regular(x))
  } else {
    x.dts <- ts_dts(x)
  }
  cname <- dts_cname(x.dts)
  setnames(x.dts, cname$time, "time")
  setnames(x.dts, cname$value, "value")

  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
  x.dts[, value := fun(time = time, value = value), by = eval(.by)]

  # x.dts <- x.dts[!is.na(value)]
  setnames(x.dts, "value", cname$value)
  setnames(x.dts, "time", cname$time)
  # setattr(x.dts, "cname", cname)
  copy_class(x.dts, x)
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


#' @name ts_pc
#' @export
ts_diff <- function(x) {
  ts_apply_time_value(x, function(time, value) {
    value - c(NA, value[-length(value)])
  })
}


# ts_pcy(AirPassengers)
# ts_diff(EuStockMarkets) - ts_diff_old(EuStockMarkets)
# ts_diff_old <- function(x) {
#   stopifnot(ts_boxable(x))
#   z <- ts_dts(x)
#   z <- ts_regular(z)
#   copy_class(z %ts-% ts_lag(z), x)
# }


#' @name ts_pc
#' @export
ts_pcy <- function(x) {
  stopifnot(ts_boxable(x))
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(((z %ts/% ts_lag(z, "1 year")) %ts-% 1) %ts*% 100, x)
}

#' @name ts_pc
#' @export
ts_diffy <- function(x) {
  stopifnot(ts_boxable(x))
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(z %ts-% ts_lag(z, "1 year"), x)
}

