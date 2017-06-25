
#' @export
#' @method ts_dts data.frame
ts_dts.data.frame <- function(x, ...){
  ts_dts(as.data.table(x, ...))
}




#' @export
#' @name ts_ts
ts_data.frame <- function (x, ...) UseMethod("ts_data.frame")

#' @export
#' @method ts_data.frame dts
ts_data.frame.dts <- function(x, ...){
  as.data.frame(ts_data.table(x, ...))
}

#' @export
#' @export
ts_tbl <-  function (x, ...) {
  stopifnot(requireNamespace("tibble"))
  tibble::as_data_frame(ts_data.table(x, ...))
}

#' @export
#' @export
ts_df <- function (x, ...) {
  ts_data.frame(x, ...)
}




# --- all methods --------------------------------------------------------------

#' @export
#' @method ts_ts data.frame
ts_ts.data.frame <- function(x, ...){
  ts_ts(ts_dts(x, ...))
}

#' @export
#' @method ts_xts data.frame
ts_xts.data.frame <- function(x, ...){
  ts_xts(ts_dts(x, ...))
}

#' @export
#' @method ts_data.frame data.frame
ts_data.frame.data.frame <- function(x, ...){
  x
}

#' @export
#' @method ts_data.table data.frame
ts_data.table.data.frame <- function(x, ...){
  ts_data.table(ts_dts(x, ...))
}













