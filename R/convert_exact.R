# --- Convertors for dectime to POSIXct and back -------------------------------


#' How many Seconds in Year?
#'
#' @param year Year
#' @param tz Time Zone
#' @examples
#' seconds_in_year(1990, tz = "")
#' @srrstats {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @noRd
seconds_in_year <- function(year, tz) {
  diff(as.numeric(seq(
    as.POSIXct(paste0(as.character(year), "-01-01"), tz = tz),
    length.out = 2, by = "1 year"
  )))
}


#' Seconds at the Start of a Year
#'
#' @param year Year
#' @param tz Time Zone
#' @examples
#' seconds_at_start_of_year(1990, tz = "")
#' @srrstats {G2.4c} *explicit conversion to character via `as.character()` (and not `paste` or `paste0`)*
#' @noRd
seconds_at_start_of_year <- function(year, tz) {
  as.numeric(as.POSIXct(paste0(as.character(year), "-01-01"), tz = tz))
}


#' Decimal Time to POSIXct
#'
#' @param x numeric, decimal time
#' @examples
#' dectime_to_POSIXct(1990.5)
#' @noRd
dectime_to_POSIXct <- function(x) {
  stopifnot(length(x) == 1L)
  year <- floor(x)
  intra <- x - year
  seconds_since_70 <-
    seconds_at_start_of_year(year, tz = "") +
    seconds_in_year(year, tz = "") * intra
  as.POSIXct(seconds_since_70, origin = "1970-01-01", tz = "")
}


#' POSIXct to Decimal Time
#'
#' @param x POSIXct
#' @examples
#' POSIXct_to_dectime(Sys.time())
#' @noRd
POSIXct_to_dectime <- function(x) {
  tz <- attributes(x)$tzone
  if (is.null(tz)) tz <- ""
  stopifnot(length(x) == 1L)
  year <- as.POSIXlt(x)$year + 1900L
  intra <- (as.numeric(x) -
    seconds_at_start_of_year(year, tz)) /
    seconds_in_year(year, tz)
  year + intra
}


# --- exact convertors ---------------------------------------------------------


#' Extract POSIXct from ts Object
#'
#' @param x ts object
#' @examples
#' ts_to_POSIXct(mdeaths)
#' @noRd
ts_to_POSIXct <- function(x) {
  stopifnot(inherits(x, "ts"))
  if (NCOL(x) > 1) x <- x[, 1]
  z <- seq(
    from = dectime_to_POSIXct(tsp(x)[1]),
    to = dectime_to_POSIXct(tsp(x)[2]),
    length.out = length(x)
  )
  z
}


#' Extract Start, End and Frequency from POSIXct
#'
#' @param x POSIXct
#' @noRd
POSIXct_to_tsp <- function(x) {
  stopifnot(inherits(x, "POSIXct"))
  dd <- diff(as.numeric(x))
  if ((max(dd) - min(dd)) > 1) {
    stop0("some dates are not equally spaced.")
  }
  start <- POSIXct_to_dectime(x[1])
  end <- POSIXct_to_dectime(x[length(x)])
  f <- (length(x) - 1) / (end - start)
  c(start, end, f)
}
