register_class("tsibble", "tbl_ts")

# to ---------------------------------------------------------------------------

ts_tsibble_dts <- function(x) {
  stopifnot(requireNamespace("tsibble"))
  cid <- dts_cname(x)$id
  ctime <- dts_cname(x)$time
  x <- dts_rm(x)

  if (length(cid) > 0){
    z <- tsibble::as_tsibble(x, key = !! cid, index = !! ctime)
  } else {
    z <- tsibble::as_tsibble(x, index = !! ctime)
  }
  z
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts tbl_ts
ts_dts.tbl_ts <- function(x) {
  stopifnot(requireNamespace("tsibble"))

  z <- as.data.table(x)

  # using tsibble meta data, we can confident about ctime
  cid <- tsibble::key_vars(x)
  measures <- tsibble::measured_vars(x)
  ctime <- setdiff(names(z), c(measures, cid))
  # browser()
  setnames(z, ctime, "time")

  if (class(z$time)[1] %in% c("yearmonth", "yearquarter", "yearweek")) {
    z$time <- as.Date(z$time)
  }

  # Ignoring non-numeric measure vars
  is.non.num <- vapply(z[, measures, with = FALSE], is.numeric, TRUE)
  measures.non.num <- measures[!is.non.num]
  if (length(measures.non.num) > 0) {
    message(
      "Ignoring non-numeric measure vars (",
      paste(measures.non.num, collapse = ", "),
      ")."
    )
    z[, (measures.non.num) := NULL]
  }

  cvalue <- setdiff(names(z), c("time", cid))


  # get rid of tsibble specifc classes, like yearweek
  if (inherits(z$time, "Date")) {
    z$time <- as.Date(z$time)
  }
  if (inherits(z$time, "POSIXct")) {
    z$time <- as.POSIXct(z$time)
  }

  if (length(cvalue) > 1) {
    # also works if 'cid' includes 'id'
    z <- melt(
      z, id.vars = c(cid, "time"), measure.vars = cvalue, variable.name = "id"
    )
    cvalue <- "value"
  }

  setcolorder(z, c(setdiff(names(z), c("time", cvalue)), "time", cvalue))
  setnames(z, "time", ctime)
  ts_dts(z)

}



# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tsibble <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tsibble") return(x)
  ts_tsibble_dts(ts_dts(x))
}
