
# since this works on multiple time series, no need to verctorize
scale_no_attr <- function(x, center = TRUE, scale = TRUE){
  z <- scale(x = x, center = center, scale = scale)
  attr(z, "scaled:center") <- NULL
  attr(z, "scaled:scale") <- NULL
  z
}

#' Normalized Time Series
#' 
#' @param x ts_boxable time series
#' @param ... further arguments
#' @export
#' @examples
#' ts_scale((ts_c(airmiles, co2, JohnsonJohnson, discoveries)))
#' ts_scale(ts_c(AirPassengers, DAX = EuStockMarkets[, 'DAX']))
ts_scale <- ts_(scale_no_attr, vectorize = TRUE)
