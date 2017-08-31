

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


# #' Aligning Time Series
# #' @param x any time series object
# #' @param with character string, the variable the remaining data is aligned with
# #' @export
# ts_align <- function(x, with){
#   if (!with %in% ts_varnames(x)){
#     stop("'", with, "' not in 'ts_varnames(x)'", call. = FALSE)
#   }
#   rng <- range(ts_select(x, vars = with)[[1]])
#   z <- ts_window(x, start = rng[1], end = rng[2])
#   ts_complete(z)
# }


# A new version, with a ts_boxable object

#' Aligning Time Series
#' @param x a ts boxable time series object
#' @param with a ts boxable time series object
#' @export
#' @examples
#' ts_align(mdeaths, fdeaths)
#' #### does not work yet ts_align(AirPassengers, fdeaths), need to implement extend = TRUE in ts_window
ts_align <- function(x, with){
  stopifnot(ts_boxable(x), ts_boxable(with))
  rng <- ts_range(with)
  z <- ts_window(x, start = rng[1], end = rng[2])
  ts_complete(z)
}



