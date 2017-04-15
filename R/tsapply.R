
#' Apply Function on Multiple Time Series
#' @param x time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param FUN a function that can applied on the corresponding object
#' @param ... additional arguments, passed to FUN
#' @export
tsapply <- function (x, FUN, ...) UseMethod("tsapply")


#' @method tsapply xts
#' @export
tsapply.xts <- function(x, FUN, ...){
  ll <- list()
  for (i in 1:NCOL(x)){
    ll[[i]] <- FUN(na.omit(x[, i]), ...)
  }
  z <- do.call("cbind", ll)
  colnames(z) <- colnames(x)
  z
}


#' @method tsapply ts
#' @export
tsapply.ts <- function(x, FUN, ...){
  if (NCOL(x) == 1) return(FUN(x, ...))
  ll <- list()
  for (i in 1:NCOL(x)){
    ll[[i]] <- FUN(na.omit(x[, i]), ...)
  }
  z <- do.call("cbind", ll)
  colnames(z) <- colnames(x)
  z

  # This should work better with internal NAs
  # as_ts(tsapply(as_xts(x), FUN, ...))
}

#' @method tsapply data.frame
#' @export
tsapply.data.frame <- function(x, FUN, ...){
  as_data.frame(tsapply(as_xts(x), FUN, ...))
}

#' @method tsapply data.table
#' @export
tsapply.data.table <- function(x, FUN, ...){
  as_data.table(tsapply(as_xts(x), FUN, ...))
}

#' @method tsapply tbl
#' @export
tsapply.tbl <- function(x, FUN, ...){
  as_tbl(tsapply(as_xts(x), FUN, ...))
}


