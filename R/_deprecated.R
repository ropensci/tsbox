#' @export
#' @name ts_span
ts_start <- function(x) {
  .Deprecated("ts_summary")
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[1]
}
#' @export
#' @name ts_span
ts_end <- function(x) {
  .Deprecated("ts_summary")
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[2]
}

