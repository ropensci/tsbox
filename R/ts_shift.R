#' Shift Time Stamp
#'
#' Shift time stamps in ts-boxable time series. Unlike
#' `ts_lag` this does not ensure regularity and also works with irregular
#' series.
#' 
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param by passed on to [base::seq.Date()]. 
#'    Argument can be specified in several ways:
#'     - A number, taken to be in days.
#'     - A object of class difftime
#'     - A character string, containing one of "day", "week", "month", "quarter" 
#'       or "year". This can optionally be preceded by a (positive or negative) 
#'       integer and a space, or followed by "s".
#' 
#' @seealso [ts_lag()], for shifting regular series. [date_shift()], for
#'   shifting `Date` vectors.
#' 
#' @return a ts-boxable time series, with the same class as the input. If time
#'  stamp shifting causes the object to be irregular, a data frame is returned.
#'
#' @examples
#' ts_shift(AirPassengers, "1 month")#' 
#' ts_shift(AirPassengers, 3)
#' 
#' @export
ts_shift <- function(x, by = NULL) {
  stopifnot(ts_boxable(x))
  z <- ts_dts(x)
  ctime <- colname_time(z)
  z[[ctime]] <- date_shift(z[[ctime]], by = by)
  copy_class(z, x)
}


