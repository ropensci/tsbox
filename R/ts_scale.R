# A blueprint for new functions? If possible, functions should work on dts, not
# on other objects. Faster and keeps time stamp intact.

#' Scale and Center Time Series
#'
#' Subtract mean (*sum(x)/n*) and divide by standard deviation
#' (*sqrt(sum(x^2)/(n-1))*). Based on [base::scale()].
#'
#' @inherit ts_default
#' @param center logical
#' @param scale logical
#' @export
#' @examples
#' \donttest{
#' ts_plot(ts_scale((ts_c(airmiles, co2, JohnsonJohnson, discoveries))))
#' ts_plot(ts_scale(ts_c(AirPassengers, DAX = EuStockMarkets[, "DAX"])))
#' }
#' @srrstats {G1.3} *All statistical terminology should be clarified and unambiguously defined.*
ts_scale <- function(x, center = TRUE, scale = TRUE) {
  value <- NULL
  z <- ts_dts(x)

  cid <- dts_cname(z)$id
  cvalue <- dts_cname(z)$value
  setnames(z, cvalue, "value")

  scale_core <- function(value) {
    z <- scale(value, center = center, scale = scale)
    attr(z, "scaled:center") <- NULL
    attr(z, "scaled:scale") <- NULL
    z
  }

  .by <- by_expr(cid)
  z[
    ,
    value := scale_core(value),
    by = eval(.by)
  ]
  setnames(z, "value", cvalue)
  ts_na_omit(copy_class(z, x))
}
