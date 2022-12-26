register_class("zooreg")

# to ---------------------------------------------------------------------------

#' Convert to Class
#' @noRd
ts_zooreg_dts <- function(x) {
  stopifnot(requireNamespace("zoo"))
  zoo::as.zoo(ts_ts(x))
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts zoo
ts_dts.zooreg <- function(x) {
  stopifnot(requireNamespace("zoo"))
  stopifnot(requireNamespace("xts"))
  ts_dts(as.ts(x))
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_zooreg <- function(x) {
  check_ts_boxable(x)
  if (relevant_class(x) == "zooreg") {
    return(x)
  }
  ts_zooreg_dts(ts_dts(x))
}
