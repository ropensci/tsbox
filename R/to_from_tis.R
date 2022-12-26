register_class("tis")

# to ---------------------------------------------------------------------------

#' Convert to Class
#' @noRd
ts_tis_dts <- function(x) {
  stopifnot(requireNamespace("tis"))
  x.ts <- ts_ts(x)
  x.tis <- tis::as.tis(x.ts)
  colnames(x.tis) <- colnames(x.ts)
  x.tis
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts tis
ts_dts.tis <- function(x) {
  stopifnot(requireNamespace("tis"))
  ts_dts(as.ts(x))
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tis <- function(x) {
  check_ts_boxable(x)
  if (relevant_class(x) == "tis") {
    return(x)
  }
  ts_tis_dts(ts_dts(x))
}
