#' Set and Variables from Multiple Time Series
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param vars character, time series to be selectd
#' @param ... arguments passed to methods
#' @examples
#' 
#' all.equal(ts_ts(mdeaths), ts_select(ts_ts(ts_c(mdeaths, fdeaths)), 'mdeaths'))
#' all.equal(ts_xts(mdeaths), 
#'           ts_select(ts_xts(ts_c(mdeaths, fdeaths)), 'mdeaths'), 
#'           check.attributes = FALSE)
#' all.equal(ts_df(mdeaths), ts_select(ts_df(ts_c(mdeaths, fdeaths)), 'mdeaths'))
#' 
#' \dontrun{
#' library(data.table)  # if you want to use the 'data.table' methods
#' all.equal(ts_dt(mdeaths), ts_select(ts_dt(ts_c(mdeaths, fdeaths)), 'mdeaths'))
#' }
#' 
#' @export
ts_select <- function (x, vars, ...) UseMethod("ts_select")

#' @export
ts_select.dts <- function(x, vars, ...){
  stopifnot(inherits(x, "dts"))
  z <- x[var %in% vars]
  add_dts_class(z)
}


#' @export
#' @rdname ts_select
#' @method ts_select ts
ts_select.ts <- function(x, vars, ...){
  if (NCOL(x) > 1) x[, vars] else x
}

#' @export
#' @rdname ts_select
#' @method ts_select xts
ts_select.xts <- function(x, vars, ...){
  z <- x[, vars]
  # we usually forget the names of single time series
  if (length(vars) ==  1) colnames(z) <- NULL
  z
}

#' @export
#' @rdname ts_select
#' @method ts_select data.frame
ts_select.data.frame <- function(x, vars, ...){
  ts_reclass(ts_select(ts_dts(x), vars = vars), x)
}


#' @export
#' @rdname ts_select
#' @method ts_select data.table
ts_select.data.table <- function(x, vars, ...){
  ts_reclass(ts_select(ts_dts(x), vars = vars), x)
}


#' @export
#' @rdname ts_select
#' @method ts_select tbl
ts_select.tbl <- function(x, vars, ...){
  ts_reclass(ts_select(ts_dts(x), vars = vars), x)
}



