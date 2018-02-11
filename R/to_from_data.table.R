# to ---------------------------------------------------------------------------

ts_data.table_dts <- function(x) {
  rm_dts_class(x)
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts data.table
ts_dts.data.table <- function(x) {
  tv <- guess_time_value(x)

  if (NCOL(x) == 2) {
    z <- copy(x)
    tvdiff <- character(0)
  } else {
    tvdiff <- setdiff(names(x), tv)
    z <- x[, c(tvdiff, tv), with = FALSE]
  }

  setnames(z, tv[1], "time")

  # setorder(z, time)
  z[, time := as_time_or_date(time)]

  # Ensure time is ordered, but var is not
  # setorder(z, ids, time) does too much
  # this also works if tvdiff is character(0)
  .by <- parse(text = paste0("list(", paste(tvdiff, collapse = ", "), ")"))
  z <- z[, .SD[order(time)], by = eval(.by)]

  setnames(z, "time", tv[1])

  add_dts_class(z)
}


# main converter ---------------------------------------------------------------

#' @name ts_ts
#' @export
ts_data.table <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "data.table") return(x)
  ts_data.table_dts(ts_dts(x))
}

#' @name ts_ts
#' @export
ts_dt <- function(x) {
  ts_data.table(x)
}
