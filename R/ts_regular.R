#' Enforce Regularity
#'
#' Enforces regularity in data frame and `xts` objects, by turning implicit
#' `NA`s into explicit `NA`s. In `ts` objects, regularity is automatically
#' enforced.
#'
#' @inherit ts_default
#' @param fill numeric, instead of `NA`, an alternative value can be specified.
#'   E.g., 0, -99.
#' @examples
#' x0 <- AirPassengers
#' x0[c(10, 15)] <- NA
#' x <- ts_na_omit(ts_dts(x0))
#' ts_regular(x)
#' ts_regular(x, fill = 0)
#'
#' m <- mdeaths
#' m[c(10, 69)] <- NA
#' f <- fdeaths
#' f[c(1, 3, 15)] <- NA
#'
#' ts_regular(ts_na_omit(ts_dts(ts_c(f, m))))
#' @export
ts_regular <- function(x, fill = NA) {
  check_ts_boxable(x)
  fill <- as.numeric(fill)
  if (length(fill) != 1) stop0("'fill' must be of length 1")

  if (inherits(x, "ts")) { # to save time
    if (!is.na(fill)) {
      x[is.na(x)] <- fill
    }
    return(x)
  }
  # standard routine
  z <- regular_core(ts_dts(x))
  if (!is.na(fill)) {
    cvalue <- dts_cname(z)$value
    z[[cvalue]][is.na(z[[cvalue]])] <- fill
  }
  copy_class(z, x)
}


#' Basic Test for Regularity
#'
#' Fast, but misses some regular series
#'
#' @param x Date or POSIXct
#'
#' @noRd
is_regular_one_basic <- function(x) {
  if (length(x) == 0L) {
    return(TRUE)
  }
  if (length(x) == 1L) {
    return(TRUE)
  }
  rng <- range(diff(as.numeric(x)))
  (rng[2] - rng[1]) < 1
}


#' Enforce Regularity
#'
#' Core function that works on dts, called by ts_regular()
#'
#' @param x data.table
#'
#' @noRd
regular_core <- function(x) {
  stopifnot(inherits(x, "dts"))

  cname <- dts_cname(x)
  ctime <- cname$time
  cid <- cname$id
  .SD <- NULL

  names.x <- copy(names(x))
  setnames(x, ctime, "time")

  regular_core_one <- function(x) {
    check_missing_time(x$time)
    if (is_regular_one_basic(x$time)) {
      return(x)
    }
    reg.time <- regularize_date(x$time)
    check_regular_pattern(reg.time)
    merge_time_date(
      data.table(time = reg.time),
      x,
      by.x = "time",
      by.y = "time"
    )
  }

  if (length(cid) == 0L) {
    z <- regular_core_one(x)
  } else {
    .by <- by_expr(cid)
    z <- x[, regular_core_one(.SD), by = eval(.by)]
  }

  setattr(z, "cname", cname)

  # resulting time column name should be ctime
  setnames(z, "time", ctime)
  # preserve original col order
  setcolorder(z, names.x)
  z
}
