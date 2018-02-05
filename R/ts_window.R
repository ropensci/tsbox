

#' Agnostic Time Windows
#' @param x any time series object
#' @param start start date, string, Date or POSIXct
#' @param end end date, string, Date or POSIXct
#' @param extend logical. If true, the start and end values are allowed to extend the series.
#' @export
ts_window <- function(x, start = NULL, end = NULL, extend = FALSE){

  # TODO fast track for ts objects, if start and end are numeric

  # 1. Coerce to specific class
  z <- ts_dts(x)

  # 2. Call specific function
  time.var <- colnames(z)[1]
  if (!is.null(start)){
    z <- filter_data.table(z, time.var, ">=", anytime(start))
  }
  if (!is.null(end)){
    z <- filter_data.table(z, time.var, "<=", anytime(end))
  } 

  # 3. and reclass
  z <- ts_reclass(z, x)

  # if (extend){
  #   z <- ts_complete(z)
  # }
 
  z
}


ts_range <- function(x){
  range(ts_dts(x)[[1]])
}


#' Aligning Time Series
#' @param x a ts boxable time series object
#' @param with a ts boxable time series object
#' @export
#' @examples
#' ts_align(mdeaths, ts_window(fdeaths, end = "1977-01-01"))
ts_align <- function(x, with){
  stopifnot(ts_boxable(x), ts_boxable(with))
  rng <- ts_range(with)
  z <- ts_window(x, start = rng[1], end = rng[2])
  ts_complete(z)
}



