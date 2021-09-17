#' Loess Trend Estimation
#'
#' Trend estimation that uses [stats::loess()].
#'
#' @inherit ts_default
#' @param ... arguments, passed to [stats::loess()]:
#' - `degree` degree of Loess smoothing
#' - `span` smoothing parameter, if `NULL`, an automated search performed (see
#'   Details)
#' @references Cleveland, William S., Eric Grosse, and William M. Shyu. "Local regression models." Statistical models in S. Routledge, 2017. 309-376.
#' @srrstats {G1.0} *Statistical Software should list at least one primary reference from published academic literature.*
#'   This is an upldated reference from ?loess
#'
#' @examples
#' \donttest{
#' ts_plot(
#'   `Raw series` = fdeaths,
#'   `Loess trend` = ts_trend(fdeaths),
#'   title = "Deaths from Lung Diseases",
#'   subtitle = "per month"
#' )
#' }
#' @export
ts_trend <- function(x, ...) {
  value <- NULL
  z <- ts_na_omit(ts_dts(x))

  predict_loess <- function(.SD, ...) {
    z <- copy(.SD)

    if (nrow(z) < 7) {
      message(
        "no trend estimation for series with less than 7 obs. ",
        "Return input series"
      )
      return(z)
    }
    value_loess <- predict(loess(
      as.numeric(value) ~ as.numeric(as.POSIXct(time)),
      ...,
      data = z
    ))
    z[, value := value_loess]
    z
  }
  z <- ts_apply_dts(z, predict_loess, ...)
  copy_class(z, x)
}
