
#' Apply Function on Multiple Time Series
#' @param x time series objects, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param FUN a function that can applied on the corresponding object
#' @param ... additional arguments, passed to FUN
#' @export
ts_apply <- function (x, FUN, ...) UseMethod("ts_apply")


#' @method ts_apply xts
#' @export
ts_apply.xts <- function(x, FUN, ...){
  ll <- list()
  for (i in 1:NCOL(x)){
    ll[[i]] <- FUN(na.omit(x[, i]), ...)
  }
  z <- do.call("cbind", ll)
  colnames(z) <- colnames(x)
  z
}


#' @method ts_apply ts
#' @export
ts_apply.ts <- function(x, FUN, ...){
  if (NCOL(x) == 1) return(FUN(x, ...))
  ll <- list()
  for (i in 1:NCOL(x)){
    ll[[i]] <- FUN(na.omit(x[, i]), ...)
  }
  z <- do.call("cbind", ll)
  colnames(z) <- colnames(x)
  z

  # This should work better with internal NAs
  # ts_ts(ts_apply(ts_xts(x), FUN, ...))
}

#' @method ts_apply data.frame
#' @export
ts_apply.data.frame <- function(x, FUN, ...){
  ts_data.frame(ts_apply(ts_xts(x), FUN, ...))
}

#' @method ts_apply data.table
#' @export
ts_apply.data.table <- function(x, FUN, ...){
  ts_data.table(ts_apply(ts_xts(x), FUN, ...))
}

#' @method ts_apply tbl
#' @export
ts_apply.tbl <- function(x, FUN, ...){
  ts_tbl(ts_apply(ts_xts(x), FUN, ...))
}


