
#' @export
#' @method ts_dts data.frame
ts_dts.data.frame <- function(x, ...){
  ts_dts(as.data.table(x, ...))
}




#' @export
#' @name ts_ts
ts_data.frame <- function (x, ...) UseMethod("ts_data.frame")

#' @export
#' @name ts_ts
#' @method ts_data.frame dts
ts_data.frame.dts <- function(x, cname = NULL, ...){
  if (is.null(cname)) cname <- deparse(substitute(x))
  as.data.frame(ts_data.table(x, cname = cname, ...))
}

#' @export
#' @name ts_ts
ts_tbl <-  function (x, cname = NULL, ...) {
  stopifnot(requireNamespace("tibble"))
  if (is.null(cname)) cname <- deparse(substitute(x))
  tibble::as_data_frame(ts_data.table(x, cname = cname, ...))
}

#' @export
#' @name ts_ts
ts_df <- function (x, cname = NULL, ...) {
  if (is.null(cname)) cname <- deparse(substitute(x))
  ts_data.frame(x, cname = cname, ...)
}




# --- all methods --------------------------------------------------------------

#' @export
#' @method ts_ts data.frame
ts_ts.data.frame <- function(x, cname = NULL, ...){
  ts_ts(ts_dts(x, ...))
}

#' @export
#' @method ts_xts data.frame
ts_xts.data.frame <- function(x, cname = NULL, ...){
  if (is.null(cname)) cname <- deparse(substitute(x))
  ts_xts(ts_dts(x, cname = cname, ...))
}

#' @export
#' @method ts_data.frame data.frame
ts_data.frame.data.frame <- function(x, cname = NULL, ...){
  if (is.null(cname)) cname <- deparse(substitute(x))
  ts_data.frame(ts_dts(x, cname = cname, ...))
}

#' @export
#' @method ts_data.table data.frame
ts_data.table.data.frame <- function(x, cname = NULL, ...){
  if (is.null(cname)) cname <- deparse(substitute(x))
  ts_data.table(ts_dts(x, cname = cname, ...))
}













