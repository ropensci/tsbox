#' @export
#' @rdname ts_pc
#' @export
ts_window <- function (x, ...) UseMethod("ts_window")

#' @export
#' @param start start date, string, Date or POSIXct
#' @param end end date, string, Date or POSIXct
#' @rdname ts_pc
#' @method ts_window xts
ts_window.xts <- function(x, start = NULL, end = NULL, ...){
  z <- x
  if (!is.null(start)){
    z <- z[paste0(as.POSIXct(start), "/")]
  }
  if (!is.null(end)){
    z <- z[paste0("/", as.POSIXct(end))]
  }
  z
}

#' @export
#' @rdname ts_pc
#' @method ts_window ts
ts_window.ts <- function(x, ...){
  ts_ts(ts_window(ts_xts(x), ...))
}


#' @export
#' @rdname ts_pc
#' @method ts_window data.frame
ts_window.data.frame <- function(x, ...){
  ts_data.frame(ts_window(ts_xts(x), ...))
}



#' @export
#' @rdname ts_pc
#' @method ts_window data.table
ts_window.data.table <- function(x, ...){
  ts_data.table(ts_window(ts_xts(x), ...))
}


#' @export
#' @rdname ts_pc
#' @method ts_window tbl
ts_window.tbl <- function(x, ...){
  ts_data.table(ts_window(ts_xts(x), ...))
}

