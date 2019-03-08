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
#' `"month"`, `"quarter" or `"year", or an abbreviation. If the series are of the same frequency, the
#' shift can be specified in periods. See examples.
#'
#' @inherit ts_dts
#' @param start start date, character string, `Date` or `POSIXct`
#' @param end end date, character string, `Date` or `POSIXct`.
#' @param template ts-boxable time series, an object of class `ts`, `xts`,
#'   `data.frame`, `data.table`, or `tibble`. If provided, `from` and `to`
#'   will be extracted from the object.
#' @return a ts-boxable time series, with the same class as the input.
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
#'}
#'
#' # Limit span of 'discoveries' to the same span as 'AirPassengers'
#' ts_span(discoveries, template = AirPassengers)
ts_span <- function(x, start = NULL, end = NULL, template = NULL) {

  if (!length(start) <= 1) {
    stop("'start' must be of length 1", call. = FALSE)
  }
  if (!length(end) <= 1) {
    stop("'end' must be of length 1", call. = FALSE)
  }

  x.dts <- ts_dts(x)
  ctime <- dts_cname(x.dts)$time
  sstr <- unique(get_shift_string(x.dts)$string)
  spl.sstr <- strsplit(sstr, split = " ")[[1]]

  # specification by period: create shift_string
  if (is.numeric(start) && start < 999){
    if (length(start) > 1){
      stop("mixed frequencies: 'start' cannot be specified as integer", call. = FALSE)
    }
    start <- paste(start * as.numeric(spl.sstr[1]), spl.sstr[2])
  }
  if (is.numeric(end) && end < 999){
    if (length(end) > 1){
      stop("mixed frequencies: 'end' cannot be specified as integer", call. = FALSE)
    }
    end <- paste(end * as.numeric(spl.sstr[1]), spl.sstr[2])
  }

  # specification by shift string: create date
  if (!is.null(start) && grepl("[a-z]", start)){
    start <- time_shift(time_shift(ts_end(x), sstr), start)
  }
  if (!is.null(end) && grepl("[a-z]", end)){
    end <- time_shift(time_shift(ts_start(x), paste0("-", sstr)), end)
  }

  # specification by template: get start and end from template
  if (!is.null(template)){
    stopifnot(is.null(start), is.null(end))
    t.dts <- ts_dts(template)
    rng <- range(t.dts[[dts_cname(t.dts)$time]])
    start <- rng[1]
    end <- rng[2]
  }

  # Outfactor in universal anytime wrapper?
  if_num_char <- function(x){
    if (inherits(x, "numeric")) {
      if (length(x) > 1) stop("numeric date input must be of length 1", call. = FALSE)
      return(as.character(x))
    }
    x
  }

  if (dts_tattr(x.dts)$class == "POSIXct") {
    anyfun <- function(x) anytime(if_num_char(x))
    # some tolerance to >= <= in seconds
    tolerance <- 0.3
  } else {
    anyfun <- function(x) anydate(if_num_char(x))
    tolerance <- 0
  }

  if (!is.null(start)) {
    .time <- anyfun(start) - tolerance
    setnames(x.dts, ctime, "time")
    x.dts <- x.dts[time >= .time]
    setnames(x.dts, "time", ctime)
  }
  if (!is.null(end)) {
    if (!is.null(start) && start > end) {
      stop("'start' cannot be at or after 'end'", call. = FALSE)
    }
    .time <- anyfun(end) + tolerance
    setnames(x.dts, ctime, "time")
    x.dts <- x.dts[time <= .time]
    setnames(x.dts, "time", ctime)
  }

  if (nrow(x.dts) == 0){
    stop("span contains no data, select different 'start' or 'end'", call. = FALSE)
  }
  z <- copy_class(x.dts, x)

  z
}

# x <- ts_dts(ts_c(fdeaths, mdeaths))
get_shift_string <- function(x){
  freq <- NULL
  x <- copy(ts_dts(x))
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)
  setnames(x, cname$time, "time")
  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
  z <- x[, list(string = frequency_one(time)$string), by = eval(.by)]
  as.data.frame(z)
}

# determine frequency of a single series
# returns list, with components freq, string
frequency_one <- function(x) {
  diffdt <- frequency_table(x)
  fm <- diffdt[which.max(freq)]
  if (fm$freq == -1) {
    udiff <- unique(diff(as.numeric(x)))
    # all.equal(max(udiff), min(udiff)) # should be 'numerically' unique
    if (!all.equal(max(udiff), min(udiff))) stop("incomplete regularization. Somehting is wrong.")
    udiff <- mean(udiff)
    # unit <- "day"
    unit <- if(inherits(x, "POSIXct")) "sec" else "day"
    nominator <- if(inherits(x, "POSIXct")) 31556952 else 365.2425
    fm$string <- paste(udiff, unit)
    fm$freq <- nominator / udiff
  }
  fm
}


#' @export
#' @name ts_span
ts_start <- function(x) {
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[1]
}
#' @export
#' @name ts_span
ts_end <- function(x) {
  x.dts <- ts_dts(x)
  range(x.dts[[dts_cname(x.dts)$time]])[2]
}
