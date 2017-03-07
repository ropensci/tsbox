
#' @export
ts_scale <- function (x, ...) UseMethod("ts_scale")

#' @export
#' @method ts_scale xts
ts_scale.xts <- function(x, ...){
  z <- scale.default(unclass(x), ...)
  reclass(z, x)
}

#' @export
#' @method ts_scale ts
ts_scale.ts <- function(x, ...){
  as_ts(ts_scale(as_xts(x), ...))
}


