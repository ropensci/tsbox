
# just an alias, but we could do more here
tsfill <- function(x){
  na.approx(x)
}

#' Utility Functions for Time Series
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param ... additional arguments, passed to methods
#' @examples
#' library(tsbox)
#' 
#' x.ts <- ts_cbind(mdeaths, fdeaths) 
#' x.xts <- ts_xts(x.ts)
#' x.df <- ts_df(x.xts)
#'
#' ts_scale(x.ts)
#' ts_scale(x.xts)
#' ts_scale(x.df)
#' ts_trend(x.ts)
#' ts_trend(x.xts)
#' ts_trend(x.df)
#' 
#' ts_pc(x.ts)
#' ts_pc(x.xts)
#' ts_pc(x.df)
#' 
#' ts_pcy(x.ts)
#' ts_pcy(x.xts)
#' ts_pcy(x.df)
#' 
#' \dontrun{
#' library(data.table)  # if you want to use the 'data.table' methods
#' x.dt <- ts_dt(x.df)
#' ts_scale(x.dt)
#' ts_trend(x.dt)
#' ts_pc(x.dt)
#' ts_pcy(x.dt)
#' }
#' 
#' @export
ts_pc <- function (x, ...) UseMethod("ts_pc")

#' @export
#' @rdname ts_pc
#' @method ts_pc ts
ts_pc.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(ts_apply(x, ts_pc))
  }
  100 * ((x / stats::lag(x, -1)) - 1)
}

#' @export
#' @rdname ts_pc
#' @method ts_pc xts
ts_pc.xts <- function(x, ...){
  ts_xts(ts_pc(ts_ts(x)))
}


#' @export
#' @rdname ts_pc
#' @method ts_pc data.frame
ts_pc.data.frame <- function(x, ...){
  ts_df(ts_pc(ts_ts(x)))
}


#' @export
#' @rdname ts_pc
#' @method ts_pc data.table
ts_pc.data.table <- function(x, ...){
  ts_dt(ts_pcy(ts_ts(x)))
}

#' @export
#' @rdname ts_pc
#' @method ts_pc tbl
ts_pc.tbl <- function(x, ...){
  ts_tbl(ts_pcy(ts_ts(x)))
}





#' @export
#' @rdname ts_pc
ts_pcy <- function (x, ...) UseMethod("ts_pcy")

#' @export
#' @rdname ts_pc
#' @method ts_pcy ts
ts_pcy.ts <- function(x, ...){
  if (NCOL(x) > 1){
    return(ts_apply(x, ts_pcy))
  }
  100 * ((x / stats::lag(x, -frequency(x))) - 1)
}

#' @export
#' @rdname ts_pc
#' @method ts_pcy xts
ts_pcy.xts <- function(x, ...){
  ts_xts(ts_pcy(ts_ts(x)))
}


#' @export
#' @rdname ts_pc
#' @method ts_pcy data.frame
ts_pcy.data.frame <- function(x, ...){
  ts_df(ts_pcy(ts_ts(x)))
}


#' @export
#' @rdname ts_pc
#' @method ts_pcy data.table
ts_pcy.data.table <- function(x, ...){
  ts_dt(ts_pcy(ts_ts(x)))
}

#' @export
#' @rdname ts_pc
#' @method ts_pcy tbl
ts_pcy.tbl <- function(x, ...){
  ts_tbl(ts_pcy(ts_ts(x)))
}

