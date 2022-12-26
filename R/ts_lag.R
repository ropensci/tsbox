#' Lag or Lead of Time Series
#'
#' Shift time stamps in ts-boxable time series, either by a number of periods or
#' by a fixed amount of time.
#'
#' The lag order, `by`, is defined the opposite way as in R base. Thus, -1 is a
#' lead and +1 a lag.
#'
#' If `by` is integer, the time stamp is shifted by the number of periods. This
#' requires the series to be regular.
#'
#' If `by` is character, the time stamp is shifted by a specific amount of time.
#' This can be one of one of `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`,
#' `"month"`, `"quarter" or `"year", optionally preceded by a (positive or
#' negative) integer and a space, or followed by plural "s". This is passed to
#' [base::seq.Date()]. This does not require the series to be regular.
#'
#' @inherit ts_default
#' @param by integer or character, either the number of shifting periods
#'   (integer), or an absolute amount of time (character). See details.
#'
#' @examples
#' \donttest{
#' ts_plot(AirPassengers, ts_lag(AirPassengers), title = "The need for glasses")
#' }
#' ts_lag(fdeaths, "1 month")
#' ts_lag(fdeaths, "1 year")
#' x <- ts_df(fdeaths)
#' ts_lag(x, "2 day")
#' ts_lag(x, "2 min")
#' ts_lag(x, "-1 day")
#' @export
ts_lag <- function(x, by = 1) {
  stopifnot(length(by) == 1L)

  value <- NULL
  .SD <- NULL

  check_ts_boxable(x)
  z <- copy(ts_dts(x))

  # numeric by only with regular series
  if (is.numeric(by)) {
    z <- ts_regular(z)
  }

  cname <- dts_cname(z)
  setnames(z, cname$time, "time")
  setnames(z, cname$value, "value")

  lag_one <- function(x) {
    check_frequency_detection(x)
    if (nrow(x) == 0L) return(x)
    x[, list(time = time_shift(time, by = by), value)]
  }
  .by <- by_expr(cname$id)
  z <- z[
    ,
    lag_one(.SD),
    by = eval(.by)
  ]

  setnames(z, "value", cname$value)
  setnames(z, "time", cname$time)
  setattr(z, "cname", cname)
  copy_class(z, x)
}
