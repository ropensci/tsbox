ts_data.table <- function (x, ...) UseMethod("ts_data.table")


#' @export
#' @method ts_dts data.table
ts_dts.data.table <- function(x, ...){
  tvv <- guess_time_var_value(x)
  z <- x[, c(tvv[['time.name']], tvv[['value.name']], tvv[['var.name']]), with = FALSE]
  setnames(z, c("time", "value", "var"))
  z[, time := anydate(time)]
  add_dts_class(z)
}

#' @export
#' @method ts_data.table dts
ts_data.table.dts <- function(x, ...){
  rm_dts_class(x)
}


#' @export
ts_dt <- function (x, ...) {
  ts_data.table(x, ...)
}



# --- all methods --------------------------------------------------------------

#' @export
#' @method ts_ts data.table
ts_ts.data.table <- function(x, ...){
  ts_ts(ts_dts(x, ...))
}

#' @export
#' @method ts_xts data.table
ts_xts.data.table <- function(x, ...){
  ts_xts(ts_dts(x, ...))
}

#' @export
#' @method ts_data.frame data.table
ts_data.frame.data.table <- function(x, ...){
  ts_data.frame(ts_dts(x, ...))
}

#' @export
#' @method ts_data.table data.table
ts_data.table.data.table <- function(x, ...){
  x
}