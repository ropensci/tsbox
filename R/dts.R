#' Internal Time Series Class
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @export
ts_dts <- function(x) {
  UseMethod("ts_dts")
}

#' @export
#' @method ts_dts dts
ts_dts.dts <- function(x) {
  x
}
