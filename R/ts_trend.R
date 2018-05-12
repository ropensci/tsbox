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

  cid <- dts_cname(z)$id
  ctime <- dts_cname(z)$time
  cvalue <- dts_cname(z)$value

  myloess <- function(y, x, ...) {
    predict(loess(
      y ~ as.numeric(as.POSIXct(x)), 
      ...
    ))
  }
 
  .by <- parse(text = paste0("list(", paste(cid, collapse = ", "), ")"))

  setnames(z, cvalue, "value")
  setnames(z, ctime, "time")

  z[
    ,
    value := myloess(y = value, x = time),
    by = eval(.by)
  ]
  
  setnames(z, "value", cvalue)
  setnames(z, "time", ctime)

  copy_class(z, x)
}

