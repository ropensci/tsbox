
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
  
  # This should work better with internal NAs
  as_ts(tsapply(as_xts(x), FUN, ...))

  # # exactly the same as above, does that make sense?
  # ll <- list()
  # for (i in 1:NCOL(x)){
  #   ll[[i]] <- FUN(na.omit(asx[, i]), ...)
  # }
  # z <- do.call("cbind", ll)
  # colnames(z) <- colnames(x)
  # z
}

