#' Loess Trend Estimation
#'
#' Trend estimation that uses [stats::loess()].
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param ... arguments, passed to [stats::loess()]:
#' - `degree` degree of Loess smoothing
#' - `span` smoothing parameter, if `NULL`, an automated search performed (see Details)
#' @examples
#' ts_plot(
#'    `Raw series` = fdeaths, 
#'    `Loess trend` = ts_trend(fdeaths),
#'    title = "Deaths from Lung Diseases",
#'    subtitle = "per month"
#' )
#' @export
ts_trend <- function(x, ...) {
  value <- NULL
  z <- ts_na_omit(ts_dts(x))

  colname.value <- colname_value(z)
  colname.time <- colname_time(z)
  colname.id <- colname_id(z)

  setnames(z, colname.value, "value")
  setnames(z, colname.time, "time")

  myloess <- function(y, x, ...) {
    predict(loess(
      y ~ as.numeric(as.POSIXct(x)), 
      ...
    ))
  }

  .by <- parse(text = paste0("list(", paste(colname.id, collapse = ", "), ")"))

  z[
    ,
    value := myloess(y = value, x = time, ...),
    by = eval(.by)
  ]

  setnames(z, "value", colname.value)
  setnames(z, "time", colname.time)

  ts_na_omit(copy_class(z, x))
}

