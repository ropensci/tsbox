#' Time Windows
#'
#' Filter series for a time range.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param start start date, character string, `Date` or `POSIXct`
#' @param end end date, character string, `Date` or `POSIXct`.
#' @return a ts-boxable time series, with the same class as the input.
#' @export
#' @examples
#' # It's fine to use an end date outside of series span
#' ts_window(mdeaths, end = "2001-01-01")
#'
#' ms <- ts_window(mdeaths, end = "1976-12-01")
#' ts_window(ts_c(fdeaths, ms), start = ts_start(ms), end = ts_end(ms))
ts_window <- function(x, start = NULL, end = NULL) {
  z <- ts_dts(x)
  ctime <- colname_time(z)

  anyfun = if (class(z[[ctime]]) == "Date") {
    function(x) anydate(x)
  } else {
    function(x) anytime(x)
  }

  if (!is.null(start)) {
    z <- filter_data.table(z, ctime, ">=", anyfun(start))
  }
  if (!is.null(end)) {
    if (!is.null(start) && start >= end) {
      stop("'start' cannot be at or after 'end'", call. = FALSE)
    }
    z <- filter_data.table(z, ctime, "<=", anyfun(end))
  }
  z <- copy_class(z, x)

  z
}

#' @export
#' @name ts_window
ts_start <- function(x) {
  xdts <- ts_dts(x)
  range(xdts[[colname_time(xdts)]])[1]
}
#' @export
#' @name ts_window
ts_end <- function(x) {
  xdts <- ts_dts(x)
  range(xdts[[colname_time(xdts)]])[2]
}
