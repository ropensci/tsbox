#' Combine to Single Time Series
#' 
#' Combine time series to a new, single time series. `ts_bind` combines time
#' series as they are, `ts_chain` chaines them together, using percentage change
#' rates.
#' 
#' @param ... one or several tsboxable time series
#' @export
ts_bind <- function(...){
  ll <- list(...)
  desired.class <- desired_class(ll)

  ll.dts <- lapply(ll, ts_dts)
  z <- Reduce(bind_dts, ll.dts)
  # setorder(z, time, var)
 
  coerce_to_(desired.class)(z)
}


# Bind two dts objects
bind_dts <- function(a, b) {
  a <- copy(a)
  b <- copy(b)

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


