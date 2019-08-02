
dts_first_of_period <- function(x) {
  smry <- ts_summary(x)
  start <- date_year(smry$start)
  end <- as.Date(paste(data.table::year(smry$end) + 1, "1", "1", sep = "-"))
  if (inherits(start, "POSIXct")) end <- as.POSIXct(end)
  time.tmpl <- data.table(time = seq(start, end, by = smry$diff))
  x1 <- x[, .(time, value)]
  x1[, has.value := TRUE]
  z <- x1[time.tmpl, roll = -Inf, on = "time"][has.value == TRUE]
  z[, has.value := NULL]
  z
}


# x <- ts_c(
#   a = ts_lag(ts_df(mdeaths), "14 days"),
#   b = ts_lag(ts_df(mdeaths), "-2 days")
# )
# ts_first_of_period(x)
# ts_first_of_period(ts_lag(ts_df(austres), "14 days"))
# x <- ts_lag(data.table(
#   time = seq(anytime::anytime(1970), length.out = 10, by = "10 sec"),
#   value = rnorm(10)
# ), "3 sec")
# ts_first_of_period(x)
ts_first_of_period <- function(x) {
  ts_apply(x, dts_first_of_period)
}

