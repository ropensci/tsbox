#' @export
ts_scale <- function(x, ...){
  x.ts <- ts_ts(x)
  z <- scale(x.ts)
  attr(z, "scaled:center") <- NULL
  attr(z, "scaled:scale") <- NULL
  z <- ts(z, start = start(x.ts), frequency = frequency(x.ts))
  ts_reclass(z, x)
}

