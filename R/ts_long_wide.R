#' Reshaping Multiple Time Series
#'
#' Functions to reshape multiple time series from 'wide' to 'long' and vice versa.
#' Note that long format data frames are ts-boxable objects, where wide format data
#' frames are not.
#'
#' @param x a ts-boxable time series, or a wide `data.frame`, 
#' `data.table`, or `tibble`.
#'
#' @return object with the same class as input
#' @examples
#' df.wide <- ts_wide(ts_df(ts_c(mdeaths, fdeaths)))
#' ts_ts(ts_long(df.wide))
#' @export
ts_long <- function(x) {
  rc <- relevant_class(x)
  if (rc %in% c("xts", "ts")) return(x)
  z <- long_core(as.data.table(x))
  copy_class(z, x)
}

# core function is also used by ts_dts.ts and ts_dts.xts
long_core <- function(x) {
  stopifnot(inherits(x, "data.table"))
  time.name <- guess_time(x)
  z <- melt(x, id.vars = time.name, variable.name = "id", variable.factor = FALSE)
  ts_dts(z)
}

#' @export
#' @name ts_long
ts_wide <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) %in% c("ts", "xts")) return(x)
  x.dts <- combine_id_cols(ts_dts(x))
  z <- wide_core(x.dts)
  # reclass
  as_class <- get(paste0("as.", relevant_class(x)))
  as_class(z)
}

wide_core <- function(x) {
  stopifnot(inherits(x, "dts"))
  if (ncol(x) == 2) return(x) # nothing to do
  # no multi id
  stopifnot(ncol(x) == 3)

  value.name <- colname_value(x)
  time.name <- colname_time(x)
  id.name <- colname_id(x)

  n.non.unique <- nrow(x) - nrow(unique(x, by = c(id.name, time.name)))
  if (n.non.unique > 0) {
    stop("contains ", n.non.unique, " duplicate entries", call. = FALSE)
  }
  z <- dcast(
    x, as.formula(paste(time.name, "~", id.name)),
    value.var = value.name
  )
  # keep order as in input
  setcolorder(z, c(time.name, unique(as.character(x[[id.name]]))))
  z
}
