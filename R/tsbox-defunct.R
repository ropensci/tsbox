#' Start and end of time series
#'
#' @inherit ts_default
#' @export
#' @name tsbox-defunct
ts_start <- function(x) {
  .Defunct("ts_summary")
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[1]
}


#' @export
#' @name tsbox-defunct
ts_end <- function(x) {
  .Deprecated("ts_summary")
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[2]
}
