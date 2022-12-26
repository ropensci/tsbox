register_class("timeSeries")

# to ---------------------------------------------------------------------------

#' Convert to Class
#' @noRd
ts_timeSeries_dts <- function(x) {
  stopifnot(requireNamespace("timeSeries"))
  stopifnot(requireNamespace("xts"))
  stopifnot(requireNamespace("zoo"))
  z <- ts_xts(x)
  dta <- zoo::coredata(z)
  timeSeries::timeSeries(dta, zoo::index(z), zone = dts_tattr(x)$tz)
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts timeSeries
ts_dts.timeSeries <- function(x) {
  stopifnot(requireNamespace("timeSeries"))
  stopifnot(requireNamespace("xts"))
  stopifnot(requireNamespace("zoo"))

  dta <- timeSeries::series(x)

  if (!grepl("%H", x@format)) {
    time <- as.Date(rownames(dta), format = x@format)
  } else {
    time <- as.POSIXct(rownames(dta), format = x@format, tz = x@FinCenter)
  }
  rownames(dta) <- NULL
  ts_dts(xts::xts(x = dta, order.by = time))
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_timeSeries <- function(x) {
  check_ts_boxable(x)
  if (relevant_class(x) == "timeSeries") {
    return(x)
  }
  ts_timeSeries_dts(ts_dts(x))
}
