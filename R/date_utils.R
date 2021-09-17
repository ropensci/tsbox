#' Shift Time Stamps
#'
#' `time_shift` adds seconds, minutes, hours, days, weeks, months, quarters or
#' years to dates.
#'
#' If `by` is character, the time stamp is shifted by a specific amount of time.
#' This can be one of `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`,
#' `"month"`, `"quarter" or `"year", or an abbreviation, optionally preceded by
#' a (positive or negative) integer and a space, or followed by plural "s". This
#' is passed to [base::seq.Date()]. This does not require the series to be
#' regular.
#'
#' @param x `Date` or `POSIXct`. If `POSIXct`, it is converted into `Date`.
#' @param by integer or character, either the number of shifting periods
#'   (integer), or an absolute amount of time (character). See details.
#'
#' @return an object of class `Date`
#'
#' @examples
#' ap.time <- ts_df(AirPassengers)$time
#'
#' head(time_shift(ap.time, 14))
#' head(time_shift(ap.time, "7 week"))
#' head(time_shift(ap.time, "-1 month"))
#'
#' time_shift(ts_summary(mdeaths)$end, 1)
#' time_shift(ts_summary(mdeaths)$end, "-14 sec")
#' time_shift(ts_summary(mdeaths)$end, "-1 year")
#' ts_span(
#'   ts_c(mdeaths, fdeaths),
#'   start = time_shift(ts_summary(mdeaths)$end, -1)
#' )
#' @srrstats {G2.3b} *Either: use `tolower()` or equivalent to ensure input of character parameters is not case dependent; or explicitly document that parameters are strictly case-sensitive.*
#'   `by` string can be entered, e.g., as `1 Month`.
#' @noRd
time_shift <- function(x, by = NULL) {
  freq <- NULL

  if (is.null(by)) {
    return(x)
  }

  # ensure input of character parameters is not case dependent
  if (is.character(by)) by <- tolower(by)

  # high freq can be added to POSIXct only
  if (is.character(by) && grepl("sec|min|hour|[^a-z]s$|[^a-z]h$", by)) {
    x <- as.POSIXct(x)
  }

  add_to_one <- function(x) seq(x, length.out = 2, by = by)[2]

  if (length(x) == 1L) {
    return(add_to_one(x))
  }

  # this is the correct, perhaps not the fastest regularity test
  # also understands monthly, quarterly etc.
  diffdt <- frequency_table(x)

  fm_na <- diffdt[is_near(freq, -1)]
  fm <- diffdt[which.max(freq)]
  if (nrow(fm_na) == 1L && fm_na$share > 0.9) {
    return(time_shift_non_heuristic(x = x, by = by))
  }

  # if series is regular, take shortcut
  # do not for "-1 day" etc. strings
  is.neg.chr.by <- is.character(by) && grepl("^\\-", by)
  if (is_near(fm$share, 1) && !is.neg.chr.by) {
    if (is.numeric(by)) {
      spl <- strsplit(diffdt$string, split = " ")[[1]]
      str <- paste(by * as.numeric(spl[1]), spl[2])
      add_to_one <- function(x) seq(x, length.out = 2, by = str)[2]
    }
    z <- seq(from = add_to_one(x[1]), by = fm$string, length.out = length(x))
    return(z)
  }

  check_numeric_by(by)

  if (inherits(x, "Date")) {
    # only do on unique values (which is faster in some cases)
    add_to_one <- function(x) seq(x, length.out = 2, by = by)[2]
    xu <- unique(x)
    zu <- do.call(c, lapply(xu, add_to_one))
    z <- merge(
      data.table(x = x),
      data.table(x = xu, z = zu),
      all.x = TRUE, sort = FALSE
    )$z
    return(z)
  }

  # shift each time stamp separately (slow)
  do.call(c, lapply(x, add_to_one))
}


#' Shift Time Stamps without Heuristics
#'
#' If heuristics don't work, this slow routine is used for shifting time stamps
#'
#' @param x `Date` or `POSIXct`. If `POSIXct`, it is converted into `Date`.
#' @param by integer or character, either the number of shifting periods
#'   (integer), or an absolute amount of time (character). See details.
#'
#' @noRd
time_shift_non_heuristic <- function(x, by) {
  xreg <- regularize_date(x)

  # regular
  if (!is.null(xreg)) {
    xreg.num <- as.numeric(xreg)
    dff <- unique(round(diff(xreg.num), 5))
    stopifnot(length(dff) == 1L)

    if (is.numeric(by)) {
      # regular, by as period
      z.num <- seq(
        from = xreg.num[1] + by * dff,
        by = dff,
        length.out = length(xreg)
      )
    } else {
      # regular, by as period
      add_to_one <- function(x) seq(x, length.out = 2, by = by)[2]
      z.num <- seq(
        from = as.numeric(add_to_one(xreg[1]), by = by),
        by = dff, length.out = length(xreg)
      )
    }

    if (inherits(x, "POSIXct")) {
      z <- as.POSIXct(z.num, origin = "1970-01-01", tz = attr(x, "tzone"))
    } else {
      z <- as.Date(z.num, origin = "1970-01-01")
    }

    return(z)
  }

  check_numeric_by(by)

  # shift each time stamp separately (slow)
  add_to_one <- function(x) seq(x, length.out = 2, by = by)[2]
  z <- (do.call(c, lapply(x, add_to_one)))

  z
}


#' First Date or POSIXct of a Year
#'
#' @param x Date or POSIXct
#'
#' @noRd
date_year <- function(x) {
  x0 <- (x)
  d <- "1"
  m <- "1"
  y <- data.table::year(x0)
  z <- as.Date(paste(y, m, d, sep = "-"))
  if (inherits(x, "POSIXct")) {
    z <- as.POSIXct(z, origin = "1970-01-01", tz = attr(x, "tzone"))
  }
  z
}
