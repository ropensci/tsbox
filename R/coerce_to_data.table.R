# currently just wraps ts_df, but could use the data.table methods for an
# efficiency gain.


#' @export
#' @rdname ts_xts
#' @import data.table
ts_data.table <- function (x, ...) {
  stopifnot(requireNamespace("data.table"))
  data.table::as.data.table(ts_data.frame(x, ...))
}


#' @export
#' @rdname ts_xts
ts_dt <- function (x, ...) {
  ts_data.table(x, ...)
}
