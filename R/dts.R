# dts time series objects

# an internally used time series class, based on data.table

# - faster than xts
# - less dependencies
# - no masking of R base

#' Internal Time Series Class
#' 
#' @param x a tsboxable object
#' @export
ts_dts <- function(x) {
  UseMethod("ts_dts")
}

#' @export
#' @method ts_dts dts
ts_dts.dts  <- function(x) {
  x
}

# ts_dts.numeric <- function(x, time, var, ...){
#   z <- data.table(time = time, value = x, var = var)
#   add_dts_class(z)
# }


