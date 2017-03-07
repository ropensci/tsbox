
#' @export
tsseas <- function (x, ...) UseMethod("tsseas")

#' @export
#' @method tsseas ts
tsseas.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(tsapply(x, tsseas, ...))
  }
  seasonal::final(seasonal::seas(x, ...))
}

#' @export
#' @method tsseas xts
tsseas.xts <- function(x, ...){
  as_xts(tsseas(as_ts(x), ...))
}

