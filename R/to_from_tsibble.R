register_class("tsibble", "tbl_ts")

# to ---------------------------------------------------------------------------

ts_tsibble_dts <- function(x) {
  stopifnot(requireNamespace("tsibble"))
  cid <- dts_cname(x)$id
  ctime <- dts_cname(x)$time
  x <- dts_rm(x)

  # for some reason, this does not work
  # tsibble::as_tsibble(x, key = id(!!cid), index = !!ctime)
  # eval parse workaround:

  if (length(cid) > 0){
    estr <- paste0(
      "tsibble::as_tsibble(x, key = tsibble::id(",
      paste(cid, collapse = ", "),
      "), index = ",
      ctime,
      ")"
    )
  } else {
    estr <- paste0(
      "tsibble::as_tsibble(x, index = ",
      ctime,
      ")"
    )
  }

  z <- eval(parse(text = estr), envir = environment())
  z
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts data.frame
ts_dts.tbl_ts <- function(x) {
  stopifnot(requireNamespace("tsibble"))

  z <- as.data.table(x)
  if (length(tsibble::measured_vars(x)) == 1){
    setnames(z, tsibble::measured_vars(x), "value")
  } else if (length(tsibble::measured_vars(x)) > 1){
    z <- ts_long(z)
  } else {
    stop("no measured vars in tsibble")
  }

  time <- setdiff(names(x), c(names(tsibble::key_vars(x)), tsibble::measured_vars(x)))

  cname <- list(id = unname(unlist(tsibble::key_vars(x))),
                time = time,
                value = "value")


  z <- dts_init(z)
  setattr(z, "cname", cname)
  z
}



# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_tsibble <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "tsibble") return(x)
  ts_tsibble_dts(ts_dts(x))
}
