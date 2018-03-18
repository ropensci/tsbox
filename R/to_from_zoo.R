register_class("zoo")

# to ---------------------------------------------------------------------------

ts_zoo_dts <- function(x) {
  stopifnot(requireNamespace("zoo"))
  zoo::as.zoo(ts_xts(x))
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts zoo
ts_dts.zoo <- function(x) {
  stopifnot(requireNamespace("zoo"))
  stopifnot(requireNamespace("xts"))
  ts_dts(xts::as.xts(x))
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_zoo <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "zoo") return(x)
  ts_zoo_dts(ts_dts(x))
}
