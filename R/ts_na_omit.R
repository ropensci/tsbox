#' Omit NA values
#'
#' Remove NA values in ts-boxable objects, turning explicit into implicit
#' missing values.
#'
#' Note that internal NAs in `ts` time series will not be removed, as this
#' conflicts with the regular structure.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @return a ts-boxable time series, with the same class as the input.
#'
#' @seealso [ts_regular], for the opposite, turning implicit into explicit missing values.
#'
#' @examples
#' x <- AirPassengers
#' x[c(2, 4)] <- NA
#'
#' # A ts object does only know explicit NAs
#' head(ts_na_omit(x))
#'
#' # by default, NAs are implicit in data frames
#' head(ts_df(x))
#' 
#' # make NAs explicit
#' head(ts_regular(ts_df(x)))
#' 
#' # and implicit again
#' head(ts_na_omit(ts_regular(ts_df(x))))
#' @export
ts_na_omit <- function(x) {
  value <- NULL
  z <- ts_dts(x)
  colname.value <- colname_value(z)
  setnames(z, colname.value, "value")
  z <- z[!is.na(value)]
  setnames(z, "value", colname.value)
  as_class(relevant_class(x))(z)
}
