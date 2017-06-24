
#' @export
ts_xts <- function (x, ...) UseMethod("ts_xts")


#' @export
#' @method ts_xts dts
ts_xts.dts <- function(x){
  stopifnot(requireNamespace("xts"))
  xts::as.xts(spread_dts(x))
}



  # x <- ts_xts(AirPassengers)
  # x <- cbind(ts_xts(AirPassengers), ts_xts(AirPassengers))


#' @export 
#' @method ts_dts xts
ts_dts.xts <- function(x){
  stopifnot(requireNamespace("xts"))

  idx <- attr(x, "index")
  dta <- as.data.frame(x, row.names = FALSE)

  if (attr(idx, "tclass") == "Date"){
    time <- as.Date(suppressWarnings(as.POSIXct(idx, origin = "1970-01-01", tz = "GMT")))
  } else if (attr(idx, "tclass") == "POSIXct"){
    time <- suppressWarnings(as.POSIXct(idx, origin = "1970-01-01", tz = "GMT"))
  }

  z <- data.table(time = time, dta)

  # if (NCOL(z) == 2){
  #   setnames(z, c("time", "value"))
  #   single.var.name <- "x"  # TODO
  #   z[, var := single.var.name]
  # }
  gather_dts(z)
}



# --- all methods --------------------------------------------------------------

#' @export
#' @method ts_ts xts
ts_ts.xts <- function(x, ...){
  ts_ts(ts_dts(x, ...))
}

#' @export
#' @method ts_xts xts
ts_xts.xts <- function(x, ...){
  x
}

#' @export
#' @method ts_data.frame xts
ts_data.frame.xts <- function(x, ...){
  ts_data.frame(ts_dts(x, ...))
}

#' @export
#' @method ts_data.table xts
ts_data.table.xts <- function(x, ...){
  ts_data.table(ts_dts(x, ...))
}

