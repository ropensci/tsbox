#' Deprecated
#' 
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param start start date, character string, `Date` or `POSIXct`
#' @param end end date, character string, `Date` or `POSIXct`.
#' @param template ts-boxable time series, an object of class `ts`, `xts`, 
#' @export
ts_window <- function(x, start = NULL, end = NULL, template = NULL){
  .Deprecated("ts_span")
  ts_span(x, start = start, end = end, template = template)
}
