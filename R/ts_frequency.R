#' Change Frequency
#'
#' Changes the frequency of a time series. By default, incomplete
#' periods of regular series are omitted.
#'
#' The [tempdisagg package](https://CRAN.R-project.org/package=tempdisagg)
#' can convert low frequency to high frequency data and
#' has support for ts-boxable objects. See
#' `vignette("hf-disagg", package = "tempdisagg")`.
#'
#' @inherit ts_default
#' @param to desired frequency, either a character string (`"year"`,
#'  `"quarter"`, `"month"`) or an integer (`1`, `4`, `12`).
#' @param aggregate character string, or function. Either `"mean"`, `"sum"`,
#'  `"first"`, or `"last"`, or any aggregate function, such as [base::mean()].
#'
#' @param na.rm logical, if `TRUE`, incomplete periods are aggregated as
#'   well. For irregular series, incomplete periods are always aggregated.
#'
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' \donttest{
#' ts_frequency(cbind(mdeaths, fdeaths), "year", "sum")
#' ts_frequency(cbind(mdeaths, fdeaths), "quarter", "last")
#'
#' ts_frequency(AirPassengers, 4, "sum")
#'
#' # Note that incomplete years are omited by default
#' ts_frequency(EuStockMarkets, "year")
#' ts_frequency(EuStockMarkets, "year", na.rm = TRUE)
#' }
#' @export
#' @srrstats {G2.3a} *Use `match.arg()` or equivalent where applicable to only permit expected values.*
#'   Used here.
#' @srrstats {G2.4a} *explicit conversion to `integer` via `as.integer()`*
ts_frequency <- function(x, to = c(
                           "year", "quarter", "month", "week", "day",
                           "hour", "min", "sec"
                         ),
                         aggregate = "mean", na.rm = FALSE) {
  check_ts_boxable(x)

  if (is.numeric(to)) {
    to <- as.integer(to)
    numeric.period <- c(month = 12L, quarter = 4L, year = 1L)
    stopifnot(to %in% numeric.period)
    to <- names(numeric.period)[numeric.period == as.integer(to)]
  }

  to <- match.arg(to)

  z <- frequency_core(ts_dts(x), to = to, aggregate = aggregate, na.rm = na.rm)
  copy_class(z, x, preserve.mode = FALSE)
}


#' Change Frequency (core function)
#'
#' @inherit ts_frequency
#'
#' @noRd
frequency_core <- function(x, to, aggregate, na.rm) {
  stopifnot(inherits(x, "dts"))

  value <- NULL

  # make sure incomplete periods result in NA
  if (na.rm == FALSE) {
    try.x <- try(ts_regular(ts_na_omit(x)), silent = TRUE)
    if (inherits(try.x, "try-error")) {
      message(
        "series is not regular, 'na.rm' is set to TRUE. ",
        "Aggregation may be based on incomplete periods"
      )
      na.rm <- TRUE
    } else {
      x <- ts_bind(NA, try.x, NA)
    }
  }
  if (is.character(aggregate)) {
    if (!aggregate %in% c("mean", "sum", "first", "last")) {
      stop0(
        "'aggregate' must be one of: 'mean', 'sum', 'first', 'last'"
      )
    }
    aggregate <- switch(aggregate,
      mean = function(x) mean(x, na.rm = na.rm),
      sum = function(x) sum(x, na.rm = na.rm),
      first = data.table::first,
      last = data.table::last
    )
  }

  if (!is.function(aggregate)) {
    stop0(
      "'aggregate' must be of class 'character' or 'function'"
    )
  }

  cname <- dts_cname(x)

  if (length(cname$id) > 0) {
    .by <- by_expr(c(cname$id, "time"))
  } else {
    .by <- by_expr("time")
  }

  x0 <- copy(x)
  data.table::setnames(x0, cname$value, "value")
  data.table::setnames(x0, cname$time, "time")

  x0$time <- lf_time(x0$time, to = to)

  z <- x0[, list(value = aggregate(value)), by = eval(.by)]
  z <- z[!is.na(value)]

  data.table::setnames(z, "value", cname$value)
  data.table::setnames(z, "time", cname$time)

  z[]
}


#' Low Frequency Time Stamps
#'
#' @param time Date or POSIXct
#' @param to desired frequency, either a character string (`"year"`,
#'  `"quarter"`, `"month"`) or an integer (`1`, `4`, `12`).
#'
#' @noRd
lf_time <- function(time, to) {
  if (to == "week") {
    # https://github.com/ropensci/tsbox/issues/183
    by <- "7 days"
    # time <- min(as.Date(time)) - 7

    first_days <- NULL
    rng <- range(as.Date(time), na.rm = TRUE)
    all_days <- data.table(time = seq(rng[1] - 7, rng[2], by = "day"))
    all_days[data.table::wday(time) == 1L, first_days := time]
    all_days[, first_days := data.table::nafill(first_days, type = "locf")]
    all_days_first_days <- all_days[!is.na(first_days)]

    time_first <-
      merge(
        data.table::data.table(time = as.Date(time)),
        all_days_first_days,
        by = "time",
        all.x = TRUE,
        sort = FALSE
      )

    z <- time_first$first_days
    return(z)
  }


  d <- data.table::mday(time)
  m <- data.table::month(time)
  y <- data.table::year(time)

  if (to == "month") {
    d <- 1L
  }

  if (to == "quarter") {
    d <- 1L
    m <- (data.table::quarter(time) - 1L) * 3L + 1L
  }

  if (to == "year") {
    d <- 1L
    m <- 1L
  }

  if (to %in% c("month", "quarter", "year")) {
    z <- as.Date(paste(y, m, d, sep = "-"))
    return(z)
  }

  # return POSIXct
  h <- data.table::hour(time)
  min <- data.table::minute(time)
  sec <- data.table::second(time)

  if (to == "min") {
    sec <- 0L
  }

  if (to == "hour") {
    sec <- 0L
    min <- 0L
  }

  if (to %in% c("hour", "min", "sec")) {
    z <- as.POSIXct(paste0(y, "-", m, "-", d, " ", h, ":", min, ":", sec))
    return(z)
  }
}
