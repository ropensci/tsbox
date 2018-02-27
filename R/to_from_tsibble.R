# to ---------------------------------------------------------------------------

ts_tsibble_dts <- function(x) {
  stopifnot(requireNamespace("tsibble"))
  cid <- colname_id(x)
  ctime <- colname_time(x)
  # for some reason, this does not work
  # tsibble::as_tsibble(x, key = id(!!cid), index = !!ctime)
  # eval parse workaround:
  estr <- paste0(
    "tsibble::as_tsibble(x, key = id(",
    paste(cid, collapse = ", "),
    "), index = ",
    ctime,
    ")"
  )
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

  
  time.var <- setdiff(names(x), c(names(tsibble::key_vars(x)), tsibble::measured_vars(x)))

  if (!identical(time.var, guess_time(z))){
    # rename if guessing does not work
    if (length(time.var) == 1){
      setnames(z, time.var, "time")
    }
  }

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
