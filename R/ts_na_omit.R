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
#' x[c(10, 15)] <- NA
#' 
#' # does not change anything for ts objects
#' ts_na_omit(x)  
#' 
#' # but turns an explicit NA into an implicit for data.frames
#' ts_na_omit(ts_dts(x))
#' @export
ts_na_omit <- function(x) {
  value <- NULL
  z <- ts_dts(x)
  as_class(relevant_class(x))(z[!is.na(value)])
}
