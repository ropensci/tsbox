#' @export
as_xts <- function (x, ...) UseMethod("as_xts")

#' @export
#' @method as_xts ts
as_xts.ts <- function(x){
  stopifnot(inherits(x, "ts"))
  m <- as.zoo(x)
  index(m) <- zoo::as.Date.yearmon(index(m))
  as.xts(m)
}


#' @export
#' @method as_xts xts
as_xts.xts <- function(x){
  x
}

