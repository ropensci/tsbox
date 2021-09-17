#' Limit Time Span
#'
#' Filter time series for a time span.
#'
#' All date and times, when entered as character strings, are processed by
#' `anytime::anydate()` or `anytime::anytime()`. Thus a wide range of inputs are
#' possible. See examples.
#'
#' `start` and `end` can be specified relative to each other,
#' using one of `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`,
#' `"month"`, `"quarter" or `"year", or an abbreviation. If the series are of
#' the same frequency, the shift can be specified in periods. See examples.
#'
#' @inherit ts_default
#' @param start start date, character string of length 1, `Date` or `POSIXct`
#' @param end end date, character string of length 1, `Date` or `POSIXct`.
#' @param template ts-boxable time series, an object of class `ts`, `xts`,
#'   `data.frame`, `data.table`, or `tibble`. If provided, `from` and `to`
#'   will be extracted from the object.
#' @param extend logical. If true, the start and end values are allowed to
#'   extend the series (by adding `NA` values).
#' @export
#' @examples
#'
#' # use 'anytime' shortcuts
#' ts_span(mdeaths, start = "1979")       # shortcut for 1979-01-01
#' ts_span(mdeaths, start = "1979-4")     # shortcut for 1979-04-01
#' ts_span(mdeaths, start = "197904")     # shortcut for 1979-04-01
#'
#' # it's fine to use an to date outside of series span
#' ts_span(mdeaths, end = "2001-01-01")
#'
#' # use strings to set start or end relative to each other
#'
#' ts_span(mdeaths, start = "-7 month")   # last 7 months
#' ts_span(mdeaths, start = -7)           # last 7 periods
#' ts_span(mdeaths, start = -1)           # last single value
#' ts_span(mdeaths, end = "1e4 hours")    # first 10000 hours
#'
#' \donttest{
#' ts_plot(
#'   ts_span(mdeaths, start = "-3 years"),
#'   title = "Three years ago",
#'   subtitle = "The last three years of available data"
#' )
#'
#' ts_ggplot(
#'   ts_span(mdeaths, end = "28 weeks"),
#'   title = "28 weeks later",
#'   subtitle = "The first 28 weeks of available data"
#' ) + theme_tsbox() + scale_color_tsbox()
#' }
#'
#' # Limit span of 'discoveries' to the same span as 'AirPassengers'
#' ts_span(discoveries, template = AirPassengers)
#' ts_span(mdeaths, end = "19801201", extend = TRUE)
#' @srrstats {G2.2} *Appropriately prohibit or restrict submission of multivariate input to parameters expected to be univariate.*
#'   Done here.
#' @srrstats {G2.0a} Provide explicit secondary documentation of any expectations on lengths of inputs
#'   Done here.
#' @srrstats {G2.0} *Implement assertions on lengths of inputs, particularly through asserting that inputs expected to be single- or multi-valued are indeed so.*
ts_span <- function(x, start = NULL, end = NULL, template = NULL,
                    extend = FALSE) {
  .SD <- NULL

  if (!length(start) <= 1) {
    stop0("'start' must be of length 1")
  }
  if (!length(end) <= 1) {
    stop0("'end' must be of length 1")
  }
  x.dts <- ts_dts(x)
  if (nrow(x.dts) == 0L) {
    return(x)
  }
  cname <- dts_cname(x.dts)
  sstr <- unique(get_shift_string(x.dts)$string)
  spl.sstr <- strsplit(sstr, split = " ")[[1]]

  tattr <- dts_tattr(x.dts)$class

  setnames(x.dts, cname$time, "time")

  # specification by period: create shift_string
  if (is.numeric(start) && start < 999) {
    if (length(sstr) > 1) {
      stop0(
        "mixed frequencies: 'start' cannot be specified as integer"
      )
    }
    start <- paste(start * as.numeric(spl.sstr[1]), spl.sstr[2])
  }
  if (is.numeric(end) && end < 999) {
    if (length(sstr) > 1) {
      stop0(
        "mixed frequencies: 'end' cannot be specified as integer"
      )
    }
    end <- paste(end * as.numeric(spl.sstr[1]), spl.sstr[2])
  }

  # specification by shift string: create date
  if (!is.null(start) && grepl("[a-zA-Z]", start)) {
    start <- time_shift(time_shift(max(x.dts$time), sstr), start)
  }
  if (!is.null(end) && grepl("[a-zA-Z]", end)) {
    end <- time_shift(time_shift(min(x.dts$time), paste0("-", sstr)), end)
  }

  # specification by date: apply anytime

  # Outfactor in universal anytime wrapper?
  if_num_char <- function(x) {
    if (inherits(x, "numeric")) {
      stopifnot(length(x) == 1)
      return(as.character(x))
    }
    x
  }
  if (tattr == "POSIXct") {
    anyfun <- function(x) anytime(if_num_char(x))
    # some tolerance to >= <= in seconds
    tolerance <- 0.3
  } else {
    anyfun <- function(x) anydate(if_num_char(x))
    tolerance <- 0
  }

  if (!is.null(start)) {
    start <- anyfun(start) - tolerance
  }
  if (!is.null(end)) {
    end <- anyfun(end) + tolerance
  }

  # specification by template: get start and end from template
  if (!is.null(template)) {
    stopifnot(ts_boxable(template))
    stopifnot(is.null(start), is.null(end))
    t.dts <- ts_dts(template)
    rng <- range(t.dts[[dts_cname(t.dts)$time]])
    start <- rng[1]
    end <- rng[2]
  }

  # ts_span(econ_us, start = 1980, extend = TRUE)
  if (extend) {
    .by <- by_expr(cname$id)
    extend_one <- function(df, end = NULL, start = NULL) {
      mystart <- min(df$time, na.rm = TRUE)
      myend <- max(df$time, na.rm = TRUE)
      if (is.null(start)) start <- mystart
      if (is.null(end)) end <- myend
      if (start > end) {
        check_start_end(start, end)
      }
      by_str <- frequency_one(df$time)$string
      # target sequence must be aligned with dates of series, not with start and date
      sq <- c(
        if (mystart > start) {
          seq(mystart, start, by = paste0("-", by_str))
        } else {
          mystart
        },
        seq(mystart, end, by = by_str)[-1]
      )
      full <- data.table(time = sq)
      z <- df[full, on = "time"]
      setorder(z, time)
      z
    }
    x.dts <- ts_na_omit(x.dts)
    x.dts <- x.dts[, extend_one(.SD, start = start, end = end), by = eval(.by)]
  }
  if (!is.null(start)) {
    x.dts <- x.dts[time >= start]
  }
  if (!is.null(end)) {
    if (!is.null(start)) {
      check_start_end(start, end)
    }
    x.dts <- x.dts[time <= end]
  }

  if (nrow(x.dts) == 0) {
    stop0(
      "span contains no data; select different 'start' or 'end'"
    )
  }

  setnames(x.dts, "time", cname$time)
  z <- copy_class(x.dts, x)

  z
}


#' Get Shift String from dts
#'
#' @param x a 'dts' object
#'
#' @examples
#' get_shift_string(ts_dts(ts_c(fdeaths, mdeaths)))
#' get_shift_string(ts_dts(EuStockMarkets))
#' @noRd
get_shift_string <- function(x) {
  freq <- NULL
  x <- copy(ts_dts(x))
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)
  setnames(x, cname$time, "time")
  .by <- by_expr(cname$id)
  z <- x[, list(string = frequency_one(time)$string), by = eval(.by)]
  as.data.frame(z)
}


#' Determine Frequency of a Single Series
#'
#' @param x Date or POSIXct
#'
#' @returns list, with components freq, string
#' @examples
#' x1 <- as.POSIXct(c("2000-01-01", "2001-01-01", "2005-03-03", "2007-03-03"))
#' x2 <- ts_tbl(AirPassengers)$time
#' x3 <- ts_na_omit(ts_tbl(ts_bind(austres, AirPassengers)))$time
#' frequency_one(x1)
#' frequency_one(x2)
#' frequency_one(x3)
#' @noRd
frequency_one <- function(x) {
  empty_fm <- data.frame(
    string = NA_character_,
    N = NA_integer_,
    freq = NA_real_,
    share = NA_real_,
    stringsAsFactors = FALSE
  )
  if (length(x) == 1L) {
    return(empty_fm)
  }
  freq <- NULL
  diffdt <- frequency_table(x)
  if (is.na(diffdt$freq[1])) diffdt$freq[1] <- -1
  fm <- diffdt[which.max(freq)]
  if (is_near(diffdt$freq[1], -1)) { # if -1 is most common value
    udiff <- unique(diff(as.numeric(x)))
    # all.equal(max(udiff), min(udiff)) # should be 'numerically' unique
    if (max(udiff) - min(udiff) > 1e5) {
      return(empty_fm)
    }
    udiff <- mean(udiff)
    # unit <- "day"
    unit <- if (inherits(x, "POSIXct")) "sec" else "day"
    nominator <- if (inherits(x, "POSIXct")) 31556952 else 365.2425
    fm$string <- paste(udiff, unit)
    fm$freq <- nominator / udiff
  }
  fm
}
