register_class("irts")

# to ---------------------------------------------------------------------------

#' Convert to Class
#' @noRd
ts_irts_dts <- function(x) {
  stopifnot(requireNamespace("tseries"))
  x.dt <- ts_wide(ts_data.table(ts_default(x)))
  time <- as.POSIXct(x.dt$time)
  data <- as.matrix(x.dt[, setdiff(names(x.dt), "time"), with = FALSE])
  tseries::irts(time, data)
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts irts
ts_dts.irts <- function(x) {
  stopifnot(requireNamespace("tseries"))
  time <- as.POSIXct(x$time)
  class(time) <- "POSIXct" # need to loose POSIXt class
  z <- data.table(time = time, x$value)
  if (ncol(z) >= 3) z <- ts_long(z)
  ts_dts(z)
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_irts <- function(x) {
  check_ts_boxable(x)
  if (relevant_class(x) == "irts") {
    return(x)
  }
  ts_irts_dts(ts_dts(x))
}
