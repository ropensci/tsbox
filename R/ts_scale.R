
scale_no_attr <- function(x, center = TRUE, scale = TRUE){
  z <- scale(x = x, center = center, scale = scale)
  attr(z, "scaled:center") <- NULL
  attr(z, "scaled:scale") <- NULL
  z
}

#' @export
ts_scale <- function(x, ...){
  z <- ts_apply_dts(ts_dts(x), scale_no_attr)
  ts_reclass(z, x)
}

