#' @param denominator positive number. Set equal to 1 if percentage change rate is
#'   given a decimal fraction
#' @name ts_index
#' @export
#' @srrstats {G2.4b} *explicit conversion to continuous via `as.numeric()`*
ts_compound <- function(x, denominator = 100) {

  not_in_data <- NULL
  value <- NULL

  denominator <- as.numeric(denominator)
  stopifnot(denominator > 0)
  stopifnot(length(denominator) == 1L)
  z <- ts_dts(x)
  d <- dts_default(z)
  z <- d$x

  z <- ts_regular(ts_na_omit(z))

  z[, value := value / denominator + 1]

  # Adding a future value to get the right length of time series
  z <- ts_bind(ts_lag(z, -1), -99999)

  .by <- by_expr(dts_cname(z)$id)
  z[
    ,
    value := c(1, cumprod(value[-length(value)])),
    by = eval(.by)
  ]
  z <- dts_restore(z, d)
  ts_na_omit(copy_class(z, x))
}


#' Indices from Levels or Percentage Rates
#'
#' `ts_index` returns an indexed series, with value of 1 at the `base` date or
#' range.
#' `ts_compound` builds an index from percentage change rates, starting with 1
#' and compounding the rates.
#'
#' @inherit ts_default
#' @param base base date, character string, `Date` or `POSIXct`, at which the
#'  index is set to 1. If two dates are provided, the mean in the range is
#'  set equal to 1 (see examples).
#' @examples
#' x <- ts_pc(ts_c(fdeaths, mdeaths))
#' ts_compound(x)
#' y <- ts_df(ts_c(fdeaths, mdeaths))
#' ts_index(y, "1974-02-01")
#' \donttest{
#' ts_plot(
#'   `My Expert Knowledge` = ts_chain(
#'     mdeaths,
#'     ts_compound(ts_bind(ts_pc(mdeaths), 15, 23, 33))
#'   ),
#'   `So Far` = mdeaths,
#'   title = "A Very Manual Forecast"
#' )
#'
#' # mean of 1974 = 1
#' ts_index(mdeaths, c("1974-01-01", "1974-12-31"))
#' }
#' @export
ts_index <- function(x, base = NULL) {
  not_in_data <- NULL
  value <- NULL
  base_value <- NULL
  .SD <- NULL
  . <- NULL

  z <- ts_dts(x)
  if (nrow(z) == 0L) return(x)
  d <- dts_default(z)
  z <- d$x

  cid <- dts_cname(z)$id
  .by <- by_expr(cid)

  if (all(is.na(d$x$value))) return(x)

  # use latest non na start point as base candidate
  if (is.null(base)) {
    dt_min_time <- z[
      !is.na(value),
      list(min.time = min(time)),
      by = eval(.by)
    ]
    base <- max(dt_min_time$min.time)
    z.base <- z[is_near(time, base), .(base_value = mean(value)), by = eval(.by)]

    # single date specification
  } else if (length(base) == 1L) {
    # let ts_span parse base and make sure it exists in data
    base <- range(ts_span(z, start = base)$time)[1]
    z.base <- z[is_near(time, base), .(base_value = mean(value)), by = eval(.by)]

    # range of dates specification (use averages)
  } else if (length(base) == 2L) {
    # let ts_span parse base and make sure it exists in data
    base <- range(ts_span(z, start = base[1], end = base[2])$time)
    z.base <- z[
      time >= base[1] & time <= base[2],
      .(base_value = mean(value)),
      by = eval(.by)
    ]
  } else {
    stop0("'base' must be of length 1 or 2, or NULL.")
  }

  if (length(cid) > 0) {
    z <- merge(z, z.base, by = cid, sort = FALSE)
  } else {
    z$base_value <- z.base$base_value
  }
  z[, value := value / base_value][, base_value := NULL]

  z <- dts_restore(z, d)
  copy_class(z, x)
}
