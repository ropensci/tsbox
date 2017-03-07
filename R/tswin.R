
#' @export
tswin <- function (x, ...) UseMethod("tswin")

#' @export
#' @method tswin xts
tswin.xts <- function(x, start = NULL, end = NULL){
  z <- x
  if (!is.null(start)){
    z <- z[paste0(as.POSIXct(start), "/")]
  }
  if (!is.null(end)){
    z <- z[paste0("/", as.POSIXct(end))]
  }
  z
}

#' @export
#' @method tswin ts
tswin.ts <- function(x, ...){
  # stop("not yet implemented")
  as_ts(tswin(as_xts(x), ...))
}


