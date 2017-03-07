
# just an alias, but we could do more here
tsfill <- function(x){
  na.approx(x)
}


#' @export
tspc <- function (x, ...) UseMethod("tspc")

#' @export
#' @method tspc ts
tspc.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(tsapply(x, tspc))
  }
  100 * ((x / stats::lag(x, -1)) - 1)
}

#' @export
#' @method tspc xts
tspc.xts <- function(x, ...){
  as_xts(tspc(as_ts(x)))
}


#' @export
#' @method tspc data.frame
tspc.data.frame <- function(x, ...){
  as_df(tspc(as_ts(x)))
}


#' @export
#' @method tspc data.table
tspc.data.table <- function(x, ...){
  as_dt(tspcy(as_ts(x)))
}






#' @export
tspcy <- function (x, ...) UseMethod("tspcy")

#' @export
#' @method tspcy ts
tspcy.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(tsapply(x, tspcy))
  }
  100 * ((x / stats::lag(x, -frequency(x))) - 1)
}

#' @export
#' @method tspcy xts
tspcy.xts <- function(x, ...){
  as_xts(tspcy(as_ts(x)))
}


#' @export
#' @method tspcy data.frame
tspcy.data.frame <- function(x, ...){
  as_df(tspcy(as_ts(x)))
}


#' @export
#' @method tspcy data.table
tspcy.data.table <- function(x, ...){
  as_dt(tspcy(as_ts(x)))
}
