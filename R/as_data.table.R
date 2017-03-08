# currently just wraps as_df, but could use the data.table methods for an
# efficiency gain.


#' @export
as_data.table <- function (x, ...) {
  stopifnot(requireNamespace("data.table"))
  as.data.table(as_df(x, ...))
}


#' @export
as_dt <- as_data.table
