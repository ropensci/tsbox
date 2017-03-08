
#' @export
#' @rdname tspc
tsscale <- function (x, ...) UseMethod("tsscale")

#' @export
#' @rdname tspc
#' @method tsscale xts
tsscale.xts <- function(x, ...){
  z <- scale.default(unclass(x), ...)
  xts::reclass(z, x)
}

#' @export
#' @rdname tspc
#' @method tsscale ts
tsscale.ts <- function(x, ...){
  as_ts(tsscale(as_xts(x), ...))
}


#' @export
#' @rdname tspc
#' @method tsscale data.frame
tsscale.data.frame <- function(x, ...){
  as_df(tsscale(as_xts(x), ...))
}

#' @export
#' @rdname tspc
#' @method tsscale data.table
tsscale.data.table <- function(x, ...){
  as_dt(tsscale(as_xts(x), ...))
}
