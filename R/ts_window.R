

#' Agnostic Time Windows
#' @param x any time series object
#' @param start start date, string, Date or POSIXct
#' @param end end date, string, Date or POSIXct
#' @export
ts_window <- function(x, start = NULL, end = NULL){

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
  ts_reclass(z, x)
}

ts_range <- function(x){
  range(ts_dts(x)[[1]])
}


#' Aligning Time Series
#' @param x any time series object
#' @param with character string, the variable the remaining data is aligned with
#' @export
ts_align <- function(x, with){
  if (!with %in% ts_varnames(x)){
    stop("'", with, "' not in 'ts_varnames(x)'", call. = FALSE)
  }
  rng <- range(ts_select(x, vars = with)[[1]])
  z <- ts_window(x, start = rng[1], end = rng[2])
  ts_complete(z)
}
