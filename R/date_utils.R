#' Manipulating Dates
#'
#' `date_shift` adds days, weeks, months, quarters or years to dates.
#' `date_month`, `date_quarter` and `date_year` return the first day of the
#' period and are useful for customized aggregation of data frames. For standard
#' aggregation, use [ts_frequency()].
#'
#' @param x `Date` or `POSIXct`. If `POSIXct`, it is converted into `Date`.
#' @param by passed on to [base::seq.Date()].
#'    Argument can be specified in several ways:
#'     - A number, taken to be in days.
#'     - A object of class difftime
#'     - A character string, containing one of "day", "week", "month", "quarter"
#'       or "year". This can optionally be preceded by a (positive or negative)
#'       integer and a space, or followed by "s".
#' @return an object of class `Date`
#' @seealso [ts_frequency()] for standard aggregation. [date_shift()], for
#'   shifting time stamps of a ts-boxable object.
#'
#' @examples
#' date_month(ts_df(AirPassengers)$time)
#' date_year(ts_df(AirPassengers)$time)
#'
#' date_shift(ts_df(AirPassengers)$time, 14)
#' date_shift(ts_df(AirPassengers)$time, "7 week")
#' date_shift(ts_df(AirPassengers)$time, "-1 month")
#'
#' @export
date_shift <- function(x, by = NULL) {
  x <- as.Date(x)
  if (is.null(by)) return(x)
  add_to_one_date <- function(x) seq(x, length.out = 2, by = by)[2]
  xu <- unique(x)
  zu <- do.call(c, lapply(xu, add_to_one_date))
  merge(data.table(x = x), data.table(x = xu, z = zu), all.x = TRUE, sort = FALSE)$z
}


#' @name date_shift
#' @export
date_month <- function(x) {
  x <- as.Date(x)
  d <- "1"
  m <- data.table::month(x)
  y <- data.table::year(x)
  as.Date(paste(y, m, d, sep = "-"))
}

#' @name date_shift
#' @export
date_quarter <- function(x) {
  x <- as.Date(x)
  d <- "1"
  m <- (data.table::quarter(x) - 1) * 3 + 1
  y <- data.table::year(x)
  as.Date(paste(y, m, d, sep = "-"))
}

#' @name date_shift
#' @export
date_year <- function(x) {
  x <- as.Date(x)
  d <- "1"
  m <- "1"
  y <- data.table::year(x)
  as.Date(paste(y, m, d, sep = "-"))
}
