


#' Use First Date of a Period
#'
#' Replace date or time values by the first of the period. tsbox usually relies
#' on timestamps being the first value of a period.
#'
#' @inherit ts_default
#'
#' @examples
#' x <- ts_c(
#'   a = ts_lag(ts_df(mdeaths), "14 days"),
#'   b = ts_lag(ts_df(mdeaths), "-2 days")
#' )
#' ts_first_of_period(x)
#' ts_first_of_period(ts_lag(ts_df(austres), "14 days"))
#' @export
ts_first_of_period <- function(x) {
  ts_apply(x, dts_first_of_period)
}


#' Use First Date of a Period (dts)
#'
#' @param x a 'dts' object
#'
#' @noRd
dts_first_of_period <- function(x) {
  check_frequency_detection(x)

  value <- NULL
  has.value <- NULL
  smry <- ts_summary(x)
  start <- date_year(smry$start)
  end <- as.Date(paste(data.table::year(smry$end) + 1, "1", "1", sep = "-"))

  if (isTRUE(smry$freq < 1)) { # e.g., decades
    start <- as.Date(
      paste(data.table::year(smry$start) %/% 10 * 10, "1", "1", sep = "-")
    )
    end <- as.Date(
      paste(
        data.table::year(smry$end) %/% 10 * 10 + 10, "1", "1", sep = "-")
      )
  }

  if (inherits(start, "POSIXct")) end <- as.POSIXct(end)
  time <- seq(start, end, by = smry$diff)
  time_ad <- time[(max(which(time <= smry$start)):min(which(time >= smry$end)))]
  time.tmpl <- data.table(time = time_ad)
  x1 <- x[, list(time, value)]
  x1[, has.value := TRUE]
  z <- x1[time.tmpl, roll = -1, on = "time"][has.value == TRUE]
  z[, has.value := NULL]
  z
}
