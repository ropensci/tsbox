

#' @export
#' @param start start date, string, Date or POSIXct
#' @param end end date, string, Date or POSIXct
#' @rdname ts_pc
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

#' @export
ts_range <- function(x){
  range(ts_dts(x)[[1]])
}


#' @export
ts_align <- function(data, with){
  if (!with %in% ts_varnames(data)){
    stop("'", with, "' not in 'ts_varnames(data)'", call. = FALSE)
  }
  rng <- range(ts_select(data, var = with)[[1]])
  z <- ts_window(data, start = rng[1], end = rng[2])
  ts_complete(z)
}
