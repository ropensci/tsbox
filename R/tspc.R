
# just an alias, but we could do more here
tsfill <- function(x){
  na.approx(x)
}

#' Convert everything to everything
#' 
#' Convert everything to everything
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param ... additional arguments, passed to methods
#' @examples
#' library(tsbox)
#' 
#' x.ts <- tsbind(mdeaths, fdeaths) 
#' x.xts <- as_xts(x.ts)
#' x.df <- as_df(x.xts)
#'
#' tsscale(x.ts)
#' tsscale(x.xts)
#' tsscale(x.df)
#' tstrend(x.ts)
#' tstrend(x.xts)
#' tstrend(x.df)
#' 
#' tspc(x.ts)
#' tspc(x.xts)
#' tspc(x.df)
#' 
#' tspcy(x.ts)
#' tspcy(x.xts)
#' tspcy(x.df)
#' 
#' \dontrun{
#' library(data.table)  # if you want to use the 'data.table' methods
#' x.dt <- as_dt(x.df)
#' tsscale(x.dt)
#' tstrend(x.dt)
#' tspc(x.dt)
#' tspcy(x.dt)
#' }
#' 
#' @export
tspc <- function (x, ...) UseMethod("tspc")

#' @export
#' @rdname tspc
#' @method tspc ts
tspc.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(tsapply(x, tspc))
  }
  100 * ((x / stats::lag(x, -1)) - 1)
}

#' @export
#' @rdname tspc
#' @method tspc xts
tspc.xts <- function(x, ...){
  as_xts(tspc(as_ts(x)))
}


#' @export
#' @rdname tspc
#' @method tspc data.frame
tspc.data.frame <- function(x, ...){
  as_df(tspc(as_ts(x)))
}


#' @export
#' @rdname tspc
#' @method tspc data.table
tspc.data.table <- function(x, ...){
  as_dt(tspcy(as_ts(x)))
}






#' @export
#' @rdname tspc
tspcy <- function (x, ...) UseMethod("tspcy")

#' @export
#' @rdname tspc
#' @method tspcy ts
tspcy.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(tsapply(x, tspcy))
  }
  100 * ((x / stats::lag(x, -frequency(x))) - 1)
}

#' @export
#' @rdname tspc
#' @method tspcy xts
tspcy.xts <- function(x, ...){
  as_xts(tspcy(as_ts(x)))
}


#' @export
#' @rdname tspc
#' @method tspcy data.frame
tspcy.data.frame <- function(x, ...){
  as_df(tspcy(as_ts(x)))
}


#' @export
#' @rdname tspc
#' @method tspcy data.table
tspcy.data.table <- function(x, ...){
  as_dt(tspcy(as_ts(x)))
}
