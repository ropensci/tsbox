
# just an alias, but we could do more here
ts_fill <- function(x){
  na.approx(x)
}


#' @export
ts_pc <- function (x, ...) UseMethod("ts_pc")

#' @export
#' @method ts_pc ts
ts_pc.ts <- function(x, ...){
  100 * ((x / stats::lag(x, -1)) - 1)
}

#' @export
#' @method ts_pc xts
ts_pc.xts <- function(x, ...){
  as_xts(ts_pc(as_ts(x)))
}


#' @export
ts_pcy <- function (x, ...) UseMethod("ts_pcy")

#' @export
#' @method ts_pcy ts
ts_pcy.ts <- function(x, ...){
  100 * ((x / stats::lag(x, -frequency(x))) - 1)
}

#' @export
#' @method ts_pcy xts
ts_pcy.xts <- function(x, ...){
  as_xts(ts_pc(as_ts(x)))
}

