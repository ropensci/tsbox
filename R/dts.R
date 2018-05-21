#' Internal Time Series Class
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `zoo`, `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, or `timeSeries`.
#' @export
ts_dts <- function(x) {
  UseMethod("ts_dts")
}

#' @export
#' @method ts_dts dts
ts_dts.dts <- function(x) {
  x
}
