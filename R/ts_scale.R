# This needs to be rewritten, to work on dt, rather than convert to ts


# since this works on multiple time series, no need to verctorize
scale_no_attr <- function(x, center = TRUE, scale = TRUE) {
  z <- scale(x = x, center = center, scale = scale)
  attr(z, "scaled:center") <- NULL
  attr(z, "scaled:scale") <- NULL
  z
}

#' Normalized Time Series
#'
#' @param x ts_boxable time series
#' @param center logical
#' @param scale logical
#' @export
#' @examples
#' ts_scale((ts_c(airmiles, co2, JohnsonJohnson, discoveries)))
#' ts_scale(ts_c(AirPassengers, DAX = EuStockMarkets[, 'DAX']))
ts_scale <- function (x, center = TRUE, scale = TRUE){
  ff <- function(x, center = TRUE, scale = TRUE) {
    stopifnot(ts_boxable(x))
    z <- scale_no_attr(ts_ts(x), center = center, scale = scale)
    copy_class(z, x, preserve.time = TRUE)
  }
  ts_apply(x, ff, center = center, scale = scale)
}

