
#' @export
tsscale <- function (x, ...) UseMethod("tsscale")

#' @export
#' @method tsscale xts
tsscale.xts <- function(x, ...){
  z <- scale.default(unclass(x), ...)
  reclass(z, x)
}

#' @export
#' @method tsscale ts
tsscale.ts <- function(x, ...){
  as_ts(tsscale(as_xts(x), ...))
}


