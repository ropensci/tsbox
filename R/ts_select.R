#' Set and Variables from Multiple Time Series
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param var character, time series to be selectd
#' @param var.name name of the column that contains `var`
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
ts_select <- function (x, var, ...) UseMethod("ts_select")

#' @export
#' @rdname ts_select
#' @method ts_select ts
ts_select.ts <- function(x, var, ...){
  if (NCOL(x) > 1) x[, var] else x
}

#' @export
#' @rdname ts_select
#' @method ts_select xts
ts_select.xts <- function(x, var, ...){
  z <- x[, var]
  # we usually forget the names of single time series
  if (length(var) ==  1) colnames(z) <- NULL
  z
}

#' @export
#' @rdname ts_select
#' @method ts_select data.frame
ts_select.data.frame <- function(x, var, var.name = getOption("tsbox.var.name", "var"), ...){
  z <- x[x[[var.name]] %in% var, ]
  if (length(var) ==  1) z[[var.name]] <- NULL
  z
}


#' @export
#' @rdname ts_select
#' @method ts_select data.table
ts_select.data.table <- function(x, var, var.name = getOption("tsbox.var.name", "var"), ...){

  # not clear: x seem to b a data.frame here
  z <- ts_data.table(x[x[[var.name]] %in% var, ])
  if (length(var) ==  1) z[[var.name]] <- NULL
  z
  # q <- parse(text = paste("variable %in%", paste(deparse(var), collapse = "")))
  # data.table:::`[.data.table`(x, eval(q))
}


#' @export
#' @rdname ts_select
#' @method ts_select tbl
ts_select.tbl <- function(x, var, var.name = getOption("tsbox.var.name", "var"), ...){
  ts_tbl(ts_select(as.data.frame(x), var = var, var.name = var.name, ...))
}



