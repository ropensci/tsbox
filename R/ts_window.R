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

  # Outfactor in universal anytime wrapper?
  if_num_char <- function(x){
    if (inherits(x, "numeric")) {
      if (length(x) > 1) stop("numeric date input must be of length 1", call. = FALSE)
      return(as.character(x))
    }
    x
  }

  if (inherits(z[[ctime]], "POSIXct")) {
    anyfun <- function(x) anytime(if_num_char(x))
  } else {
    x <- if_num_char(x)
    anyfun <- function(x) anydate(if_num_char(x))
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
  if (nrow(z) == 0){
    stop("window contains no data, select different 'start' or 'end'", call. = FALSE)
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
