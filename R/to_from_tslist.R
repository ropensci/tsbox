# to ---------------------------------------------------------------------------

ts_tslist_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  if (number_of_series(x) == 1) {
    z <- ts_ts(x)
    # z <- list(x = ts_ts(x))
  } else {
    x <- combine_id_cols(x)
    z <- lapply(split(x, x$id), ts_ts)
  }
  class(z) <- c("tslist", "list")
  z
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts xts
ts_dts.tslist <- function(x) {
  ts_dts(rbindlist(lapply(x, ts_dts), idcol = "id"))
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tslist <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tslist") return(x)
  ts_tslist_dts(ts_dts(x))
}
