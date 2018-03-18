register_class("xts")

# to ---------------------------------------------------------------------------

ts_xts_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  stopifnot(requireNamespace("xts"))
  z <- wide_core(combine_id_cols(x))
  xts::xts(x = as.matrix(z[, -1]), order.by = z[[1]])
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts xts
ts_dts.xts <- function(x) {
  stopifnot(requireNamespace("xts"))

  idx <- attr(x, "index")
  tclass <- attr(idx, "tclass")
  attributes(idx) <- NULL

  dta <- as.data.frame(x, row.names = FALSE)
  if (tclass[1] == "Date") {
    time <- as.Date(as.POSIXct(idx, origin = "1970-01-01"))
  } else if (tclass[1] == "POSIXct") {
    time <- as.POSIXct(idx, origin = "1970-01-01")
  }
  dta <- data.table(time = time, dta)
  if (NCOL(dta) == 2) {
    setnames(dta, c("time", "value"))
  } else {
    dta <- melt(dta, id.vars = "time", variable.name = "id", variable.factor = FALSE)
    setcolorder(dta, c("id", "time", "value"))
  }
  dts_init(dta)
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_xts <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "xts") return(x)
  ts_xts_dts(ts_dts(x))
}
