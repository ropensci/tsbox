# currently just wraps as_df, but could use the data.table methods for an
# efficiency gain.


#' @export
#' @rdname as_xts
as_data.table <- function (x, ...) {
  stopifnot(requireNamespace("data.table"))
  data.table::as.data.table(as_df(x, ...))
}


#' @export
#' @rdname as_xts
as_dt <- as_data.table <- function (x, ...) {
  data.table::as.data.table(x, ...)
}
