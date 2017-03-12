#' Set and Variables from Multiple Time Series
#' 
#' @param x a time series object, either `ts`, `xts`, `data.frame` or `data.table`.
#' @param var character, time series to be selectd
#' @examples
#' 
#' all.equal(as_ts(mdeaths), tsselect(as_ts(tsbind(mdeaths, fdeaths)), 'mdeaths'))
#' all.equal(as_xts(mdeaths), tsselect(as_xts(tsbind(mdeaths, fdeaths)), 'mdeaths'), check.attributes = FALSE)
#' all.equal(as_df(mdeaths), tsselect(as_df(tsbind(mdeaths, fdeaths)), 'mdeaths'))
#' 
#' \dontrun{
#' library(data.table)  # if you want to use the 'data.table' methods
#' all.equal(as_dt(mdeaths), tsselect(as_dt(tsbind(mdeaths, fdeaths)), 'mdeaths'))
#' }
#' 
#' @export
tsselect <- function (x, var) UseMethod("tsselect")

#' @export
#' @rdname tsselect
#' @method tsselect ts
tsselect.ts <- function(x, var){
  if (NCOL(x) > 1) x[, var] else x
}

#' @export
#' @rdname tsselect
#' @method tsselect xts
tsselect.xts <- function(x, var){
  z <- x[, var]
  # we usually forget the names of single time series
  if (length(var) ==  1) colnames(z) <- NULL
  z
}

#' @export
#' @rdname tsselect
#' @method tsselect data.frame
tsselect.data.frame <- function(x, var){
  z <- subset(x, variable %in% var)
  if (length(var) ==  1) z[['variable']] <- NULL
  z
}


#' @export
#' @rdname tsselect
#' @method tsselect data.table
tsselect.data.table <- function(x, var){

  # not clear: x seem to b a data.frame here
  z <- as_data.table(subset(x, variable %in% var))
  if (length(var) ==  1) z[['variable']] <- NULL
  z
  # q <- parse(text = paste("variable %in%", paste(deparse(var), collapse = "")))
  # data.table:::`[.data.table`(x, eval(q))
}




