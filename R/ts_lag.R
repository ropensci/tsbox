#' Lag or Lead of Time Series
#'
#' Shift time stamps in ts-boxable time series, either by a number of periods or
#' by a fixed amount of time.
#'
#' The lag order, `by`, is defined as in R base. Thus, -1 is a lag and +1 a lead
#' (dplyr uses an opposite definition). 
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
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param by integer or character, either the number of shifting periods
#'   (integer), or an absolute amount of time (character). See details.
#'
#' @seealso [ts_lag()], for shifting regular series. [date_shift()], for
#'   shifting `Date` vectors.
#'
#' @return a ts-boxable time series, with the same class as the input. If time
#'  stamp shifting causes the object to be irregular, a data frame is returned.
#'
#' @examples
#' ts_plot(AirPassengers, ts_lag(AirPassengers))
#' head(ts_lag(AirPassengers, "1 month"))
#' head(ts_lag(AirPassengers, "1 year"))
#' head(ts_lag(ts_df(AirPassengers), "2 day"))
#' # head(ts_lag(ts_df(AirPassengers), "2 min")) not yet working
#' @export
ts_lag <- function(x, by = 1) {

  stopifnot(length(by) == 1)

  value <- NULL

  stopifnot(ts_boxable(x))
  z <- ts_dts(x)
  
  if (is.character(by)){
    ctime <- colname_time(z)
    z[[ctime]] <- date_shift(z[[ctime]], by = by)
    return(copy_class(z, x))
  }

  z <- ts_regular(z)

  colname.id <- colname_id(z)
  colname.value <- colname_value(z)
  colname.time <- colname_time(z)

  setnames(z, colname.time, "time")
  setnames(z, colname.value, "value")

  lag_one_by_period <- function(x){
    diffdt <- frequency_table(x$time)
    # this is what we expect from a dts that went through ts_regular
    stopifnot(nrow(diffdt) == 1)
    stopifnot(diffdt$share == 1)
    
    spl <- strsplit(diffdt$string, split = " ")[[1]]
    str <- paste(by * as.numeric(spl[1]), spl[2])

    ts_lag(x, by = str)
  }

  .by <- parse(text = paste0("list(", paste(colname.id, collapse = ", "), ")"))
  z <- z[
    ,
    lag_one_by_period(.SD),
    by = eval(.by)
  ]
  setnames(z, "value", colname.value)
  setnames(z, "time", colname.time)
  copy_class(z, x)
}

