#' @name ts_ts
#' @export
ts_data.table <- function (x, ...) UseMethod("ts_data.table")




as_time_or_date <- function(x){
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct")) {
    return(x)
  }

  # We want to return a date unless its really a time....
  anydate(as.character(x))

  # anytime(as.character(x))
}

#' @export
#' @method ts_dts data.table
ts_dts.data.table <- function(x, ...){
  
  tv <- guess_time_value(x)

  if (NCOL(x) == 2){
    z <- copy(x)
  } else {
    tvdiff <- setdiff(names(x), tv)
    z <- x[, c(tvdiff, tv), with = FALSE]
  }

  setnames(z, tv[1], "time")
  # TODO ensure time is ordered, but var is not! The following does too much:
  # setorder(z, var, time)  
  z[, time := as_time_or_date(time)]
  setnames(z, "time", tv[1])

  add_dts_class(z)
}

#' @export
#' @method ts_data.table dts
ts_data.table.dts <- function(x, ...){
  rm_dts_class(x)
}


#' @export
#' @name ts_ts
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