
#' @export
tsapply <- function (x, ...) UseMethod("tsapply")


#' @export
#' @method tsapply xts
tsapply.xts <- function(x, FUN, ...){
  stopifnot(inherits(x, "xts"))
  ll <- list()
  for (i in 1:NCOL(x)){
    ll[[i]] <- FUN(na.omit(x[, i]), ...)
  }
  z <- do.call("cbind", ll)
  colnames(z) <- colnames(x)
  z
}




#' @export
#' @method tsapply ts
tsapply.ts <- function(x, FUN, ...){
  ll <- list()
  for (i in 1:NCOL(x)){
    ll[[i]] <- FUN(na.omit(x[, i]), ...)
  }
  z <- do.call("cbind", ll)
  colnames(z) <- colnames(x)
  z
}

