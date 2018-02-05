
# to ---------------------------------------------------------------------------

ts_tbl_dts <- function(x){
  stopifnot(requireNamespace("tibble"))
  tibble::as_data_frame(as.data.frame(ts_data.table(x)))
}


# from -------------------------------------------------------------------------

# not needed, uses ts_dts.data.frame


# main converter ---------------------------------------------------------------

# could be probably automated

#' @name ts_ts
#' @export
ts_tbl <- function(x){
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tbl") return(x)
  ts_tbl_dts(ts_dts(x))
}
