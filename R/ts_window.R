#' Time Windows
#'
#' Filter series for time range.
#'
#' All date and times, when entered as charachter strings, are processed by
#' `anytime::anydate()` or `anytime::anytime()`. Thus a wide range of inputs are
#' possible. See examples.
#' 
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param start start date, character string, `Date` or `POSIXct`
#' @param end end date, character string, `Date` or `POSIXct`.
#' @param template ts-boxable time series, an object of class `ts`, `xts`, 
#'   `data.frame`, `data.table`, or `tibble`. If provided, `start` and `end` 
#'   will be extracted from the object.
#' @return a ts-boxable time series, with the same class as the input.
#' @export
#' @examples
#' 
#' # use 'anytime' shortcuts
#' ts_window(mdeaths, start = "1979")       # shortcut for 1979-01-01
#' 
#' ts_window(mdeaths, start = "1979-4")     # shortcut for 1979-04-01
#' 
#' ts_window(mdeaths, start = "197904")     # shortcut for 1979-04-01
#' 
#' # it's fine to use an end date outside of series span
#' ts_window(mdeaths, end = "2001-01-01")
#'
#' # ts_end and ts_start, together with time_shift allow flexible specification
#' # of relative ranges:
#' 
#' # latest value
#' ts_window(mdeaths, start = time_shift(ts_end(mdeaths), -1))
#' 
#' ts_plot(
#'   ts_window(mdeaths, start = time_shift(ts_end(mdeaths), "-3 years")),
#'   title = "Three years",
#'   subtitle = "The last three years of data"
#' )
#' 
#' ts_ggplot(
#'   ts_window(mdeaths, end = time_shift(ts_start(mdeaths), "28 weeks")),
#'   title = "28 weeks later",
#'   subtitle = "The first 28 weeks of data"
#' ) + theme_tsbox() + scale_color_tsbox()
#' 
#' # Limit span of 'discoveries' to the same span as 'AirPassengers'
#' ts_window(discoveries, template = AirPassengers)
ts_window <- function(x, start = NULL, end = NULL, template = NULL) {
  x.dts <- ts_dts(x)
  ctime <- colname_time(x.dts)

  if (!is.null(template)){
    xdts <- ts_dts(template)
    rr <- range(xdts[[colname_time(xdts)]])
    start <- rr[1]
    end <- rr[2]
  }

  # Outfactor in universal anytime wrapper?
  if_num_char <- function(x){
    if (inherits(x, "numeric")) {
      if (length(x) > 1) stop("numeric date input must be of length 1", call. = FALSE)
      return(as.character(x))
    }
    x
  }

  if (inherits(x.dts[[ctime]], "POSIXct")) {
    anyfun <- function(x) anytime(if_num_char(x))
  } else {
    anyfun <- function(x) anydate(if_num_char(x))
  }

  if (!is.null(start)) {
    x.dts <- filter_data.table(x.dts, ctime, ">=", anyfun(start))
  }
  if (!is.null(end)) {
    if (!is.null(start) && start >= end) {
      stop("'start' cannot be at or after 'end'", call. = FALSE)
    }
    x.dts <- filter_data.table(x.dts, ctime, "<=", anyfun(end))
  }

  if (nrow(x.dts) == 0){
    stop("window contains no data, select different 'start' or 'end'", call. = FALSE)
  }
  z <- copy_class(x.dts, x)

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
