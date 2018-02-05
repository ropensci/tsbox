#' Combine to Single Time Series
#' 
#' Combine time series to a new, single time series. `ts_bind` combines time
#' series as they are, `ts_chain` chaines them together, using percentage change
#' rates.
#' 
#' @param ... one or several tsboxable time series
#' @examples
#' ts_bind(ts_window(mdeaths, end = "1975-12-01"), fdeaths)
#' ts_bind(mdeaths, c(2, 2))
#' ts_bind(mdeaths, 3, ts_bind(fdeaths, c(99, 2)))
#' @export
ts_bind <- function(...){
  ll <- list(...)

  tsboxable <- vapply(ll, ts_boxable, TRUE)
  desired.class <- desired_class(ll[tsboxable])

  # ll.dts <- lapply(ll, ts_dts)
  z <- Reduce(bind_two, ll)
  # setorder(z, time, var)
 
  coerce_to_(desired.class)(z)
}


# Bind two dts objects
bind_two <- function(a, b) {
  a <- ts_dts(copy(a))

  if (!ts_boxable(b)){
    # this can be done prettier once rdts are worked out. For now, using 
    # ts obejcts for regularization
    stopifnot(is.numeric(b))
    a.ts <- ts_ts(a)
    b <- ts(c(a.ts, b), start = start(a.ts), frequency = frequency(a.ts))
  }

  b <- ts_dts(copy(b))

  stopifnot(inherits(a, "dts"), inherits(b, "dts"))
  
  colname.value <- colname_value(a) 
  colname.time <- colname_time(a) 
  colname.id <- colname_id(a) 

  # temporary, rename back at the end
  setnames(a, colname.time, "time")
  setnames(b, colname_time(b), "time")

  setnames(a, colname.value, "value")
  setnames(b, colname.value, "value_b")

  if (!identical(colname.id, colname_id(b))) {
    stop(
      "Series do not have the same ids: ", 
      paste(colname.id, collapse = ", "), 
      "and", 
      paste(colname_id(b), collapse = ", ")
    )
  }

  z <- merge(a, b, by = c(colname.id, colname.time), all = TRUE)
  # remove key added by merge
  setkey(z, NULL)
  z <- z[is.na(value), value := value_b]
  z[, value_b := NULL]

  # canonical col order
  setcolorder(z, c(setdiff(names(z), c("time", "value")), c("time", "value")))
  
  setnames(z, "time", colname.time)
  setnames(z, "value", colname.value)
  z[]
 
}


