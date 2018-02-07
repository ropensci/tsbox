#' Omit NA values
#' @param x any time series object
#' @export
ts_na_omit <- function(x) {
  value <- NULL
  z <- ts_dts(x)
  as_class(relevant_class(x))(z[!is.na(value)])
}
