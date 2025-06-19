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
#' `r lifecycle::badge('deprecated')`
ts_end <- function(x) {
  lifecycle::deprecate_warn(
    "0.1.0",
    "tsbox::ts_end(x)",
    "tsbox::ts_summary(x)$end"
  )
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[2]
}
