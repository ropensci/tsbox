
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


#' @export
#' @method tsscale data.frame
tsscale.data.frame <- function(x, ...){
  as_df(tsscale(as_xts(x), ...))
}

#' @export
#' @method tsscale data.table
tsscale.data.table <- function(x, ...){
  as_dt(tsscale(as_xts(x), ...))
}
