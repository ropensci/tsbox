#' Loess Trend Estimation
#'
#' Trend estimation that uses [stats::loess()].
#'
#' @inherit ts_dts
#' @param ... arguments, passed to [stats::loess()]:
#' - `degree` degree of Loess smoothing
#' - `span` smoothing parameter, if `NULL`, an automated search performed (see Details)
#' @examples
#' \donttest{
#' ts_plot(
#'    `Raw series` = fdeaths,
#'    `Loess trend` = ts_trend(fdeaths),
#'    title = "Deaths from Lung Diseases",
#'    subtitle = "per month"
#' )
#' }
#' @export
ts_trend <- function(x, ...) {
  value <- NULL
  z <- ts_na_omit(ts_dts(x))

  predict_loess <- function(.SD, ...) {
    z <- copy(.SD)
    value_loess <- predict(loess(
      value ~ as.numeric(as.POSIXct(time)),
      ...,
      data = .SD
    ))
    z[, value := value_loess]
    z
  }

  ts_apply_dts(z, predict_loess,...)
}

