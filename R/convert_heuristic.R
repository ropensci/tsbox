#' Extract Date or POSIXct from ts Object
#'
#' @param x ts object
#' @examples
#' ts_to_date_time(mdeaths)
#' @noRd
ts_to_date_time <- function(x) {
  freq <- NULL

  stopifnot(inherits(x, "ts"))

  # if 'mts', only consider first column
  if (NCOL(x) > 1) x <- x[, 1]

  # add 1e-8 to avoid rounding problems #186
  first.year <- (tsp(x)[1] + 1e-8) %/% 1
  first.subperiod <- (tsp(x)[1] + 1e-8) %% 1
  fr <- frequency(x)

  # we did not allow an offset, but this is wrong. offset is common, e.g., for
  # weekly data.
  # division <- first.subperiod / (1 / fr)
  # offset <- division - round(division)
  # stopifnot(abs(offset) < 1e-3)

  md <- meta_freq()[is_near(freq, fr)]

  # non heuristic conversion for non-heuristics
  if (nrow(md) == 0L) {
    z <- ts_to_POSIXct(x)

    # heuristic high freq > 12
  } else if (is_near(md$freq, 365.2425)) {
    # to improve accuracy for daily data, treat them separately
    # (also seedate_time_to_tsp)

    start.ts <- tsp(x)[1]
    start.year <- floor(start.ts)
    sq.ts <- ts(integer(10), start = start.ts, frequency = 365.2425)
    sq.ts.ext <- window(sq.ts, start = floor(start(sq.ts)), extend = TRUE)
    first.obs <- which(!is.na(sq.ts.ext))[1]

    start.time <- as.Date(paste0(floor(floor(start.ts)), "-01-01"))
    end.time <- as.Date(paste0(floor(floor(start.ts) + 1), "-01-01"))
    sq.time <- seq(start.time, end.time, by = "1 day")
    start <- sq.time[first.obs]

    z <- seq(
      from = start,
      by = "1 day",
      length.out = length(x)
    )
  } else if (md$freq > 12) {
    stopifnot(inherits(x, "ts"))
    if (NCOL(x) > 1) x <- x[, 1]
    start <- dectime_to_POSIXct(tsp(x)[1])

    # causes conversion to POSIXlt, which messes daylight saving time
    # start <- round(start, "secs")

    # daily data should be stored as Date
    if (identical(md$string, "1 day")) {
      start <- as.Date(start)
    }

    z <- seq(
      from = start,
      by = md$string,
      length.out = length(x)
    )

    # heuristic low freq <= 12
  } else {
    month.per.unit <- 12 / fr
    first.month <- round((first.subperiod * fr) * month.per.unit + 1)
    first.Date <- as.Date(ISOdate(
      year = first.year,
      month = first.month,
      day = 1
    ))
    stopifnot(!is.na(first.Date))
    z <- seq.Date(first.Date, length.out = length(x), by = md$string)
  }

  z
}

#' Extract Start, End and Frequency from Date or POSIXct
#'
#' @param x ts object
#' @param frequency if missing it is detected from x
#' @noRd
date_time_to_tsp <- function(x, frequency = NULL) {
  freq <- NULL

  if (is.null(frequency)) {
    check_frequency_detection(x)
    frequency <- unique(frequency_table(x)$freq)
    if (length(frequency) != 1L) {
      stop0("sequence is not regular. Use ts_regular().")
    }
  }
  # Non heuristic conversion
  if (is_near(frequency, -1)) {
    z <- POSIXct_to_tsp(as.POSIXct(x))
    # Low frequency conversion
  } else if (frequency <= 12) {
    if (inherits(x, "POSIXct")) {
      x <- as.Date(x)
    }
    st <- as.POSIXlt(x[1])
    y <- st$year + 1900L
    m <- st$mon + 1L
    d <- st$mday
    start <- y
    if (is_near(frequency, 4)) start <- c(y, ((m - 1) / 3) + 1)
    if (is_near(frequency, 12)) start <- c(y, m)
    if (d != 1L) {
      stop0(
        "time column must be specified as the first date of the period"
      )
    }
    z <- tsp(ts(x, frequency = frequency, start = start)) # a bit inefficient
  } else if (is_near(frequency, 365.2425)) {
    # to improve accuracy for daily data, do not use non heuristic conversion

    md <- meta_freq()[is_near(freq, frequency)]
    str <- md$str

    start.time <- date_year(x[1])
    end.time <- time_shift(start.time, "1 year")

    sq.time0 <- seq(start.time, end.time, by = str)
    sq.time <- sq.time0[-length(sq.time0)]

    sq.ts <- ts(sq.time, frequency = frequency, start = data.table::year(x[1]))
    start <- time(sq.ts)[sq.ts >= as.integer(x[1])][1]

    z <- tsp(ts(x, start = start, frequency = frequency))
  } else {

    # non heuristic converson
    md <- meta_freq()[is_near(freq, frequency)]
    z <- tsp(
      ts(0, start = POSIXct_to_dectime(as.POSIXct(x[1])), frequency = frequency)
    )
  }
  z
}
