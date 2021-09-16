#' Internal Time Series Class
#'
#' @inherit ts_default
#' @export
ts_dts <- function(x) {
  check_ts_boxable(x)
  UseMethod("ts_dts")
}

#' @export
#' @method ts_dts dts
ts_dts.dts <- function(x) {
  x
}
