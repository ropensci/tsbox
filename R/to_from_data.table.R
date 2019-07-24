register_class("data.table")

# to ---------------------------------------------------------------------------

ts_data.table_dts <- function(x) {
  dts_rm(x)[]
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts data.table
ts_dts.data.table <- function(x) {
  dts_init(x)
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_data.table <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "data.table") return(x)
  ts_data.table_dts(ts_dts(x))
}

#' @name ts_ts
#' @export
ts_dt <- function(x) {
  ts_data.table(x)
}
