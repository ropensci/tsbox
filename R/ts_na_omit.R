#' Omit NA values
#'
#' Remove NA values in ts-boxable objects, turning explicit into implicit
#' missing values.
#'
#' Note that internal NAs in `ts` time series will not be removed, as this
#' conflicts with the regular structure.
#'
#' @inherit ts_default
#'
#' @seealso [ts_regular], for the opposite, turning implicit into explicit
#'   missing values.
#'
#' @examples
#' x <- AirPassengers
#' x[c(2, 4)] <- NA
#'
#' # A ts object does only know explicit NAs
#' ts_na_omit(x)
#'
#' # by default, NAs are implicit in data frames
#' ts_df(x)
#'
#' # make NAs explicit
#' ts_regular(ts_df(x))
#'
#' # and implicit again
#' ts_na_omit(ts_regular(ts_df(x)))
#' @export
ts_na_omit <- function(x) {
  value <- NULL
  z <- ts_dts(x)
  if (inherits(x, "dts")) z <- copy(z)
  cname <- dts_cname(z)
  cvalue <- cname$value
  setnames(z, cvalue, "value")
  z <- z[!is.na(value)]
  setnames(z, "value", cvalue)
  setattr(z, "cname", cname)
  as_class(relevant_class(x))(z)
}
