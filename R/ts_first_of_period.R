
dts_first_of_period <- function(x) {
  value <- NULL
  has.value <- NULL

  x <- ts_na_omit(x)
  smry <- ts_summary(x)
  start <- date_year(smry$start)
  end <- as.Date(paste(data.table::year(smry$end) + 1, "1", "1", sep = "-"))

  if (smry$freq < 1) {  # e.g., decades
    start <- as.Date(paste(data.table::year(smry$start) %/% 10 * 10, "1", "1", sep = "-"))
    end <- as.Date(paste(data.table::year(smry$end) %/% 10 * 10 + 10, "1", "1", sep = "-"))
  }

  if (inherits(start, "POSIXct")) end <- as.POSIXct(end)
  time <- seq(start, end, by = smry$diff)
  time_adj <- time[(max(which(time <= smry$start)):min(which(time >= smry$end)))]
  time.tmpl <- data.table(time = time_adj)
  x1 <- x[, list(time, value)]
  x1[, has.value := TRUE]
  z <- x1[time.tmpl, roll = -Inf, on = "time"][has.value == TRUE]
  z[, has.value := NULL]
  z
}


#' Use First Date of a Period
#'
#' Replace date or time values by the first of the period. tsbox usually relies
#' on timestamps being the first value of a period.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `zoo`,
#'   `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`, `irts` or
#'   `timeSeries`.
#' @return A ts-boxable object of the same class as the input.
#'
#' @examples
#' x <- ts_c(
#'   a = ts_lag(ts_df(mdeaths), "14 days"),
#'   b = ts_lag(ts_df(mdeaths), "-2 days")
#' )
#' ts_first_of_period(x)
#' ts_first_of_period(ts_lag(ts_df(austres), "14 days"))
#' x <- ts_lag(data.frame(
#'   time = seq(anytime::anytime(1970), length.out = 10, by = "10 sec"),
#'   value = rnorm(10)
#' ), "3 sec")
#' ts_first_of_period(x)
#' @export
ts_first_of_period <- function(x) {
  ts_apply(x, dts_first_of_period)
}

