register_class("tbl", "tbl_df")

# to ---------------------------------------------------------------------------

ts_tbl_dts <- function(x) {
  stopifnot(requireNamespace("tibble"))
  tibble::as_tibble(as.data.frame(ts_data.table(x)))
}

as.tbl_df <- function(x) {
  stopifnot(requireNamespace("tibble"))
  tibble::as_tibble(x)
}

# from -------------------------------------------------------------------------

# not needed, uses ts_dts.data.frame


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tbl <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tbl") return(x)
  ts_tbl_dts(ts_dts(x))
}
