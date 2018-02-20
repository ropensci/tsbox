ts_to_date_time <- function(x) {

  freq <- NULL 
  
  stopifnot(inherits(x, "ts"))

  # if 'mts', only consider first column
  if (NCOL(x) > 1) x <- x[, 1]

  first.year <- tsp(x)[1] %/% 1
  first.subperiod <- tsp(x)[1] %% 1
  fr <- frequency(x)

  division <- first.subperiod / (1 / fr)

  if (abs(division - round(division)) > 1e-5) {
    stop(
      "subperiod is not dividable by frequency\n\n",
      "If you encounter this rare rounding issue, many thanks for ",
      "reporting a reproducible example on:",
      "\n\n    https://github.com/christophsax/tsbox\n",
      call. = FALSE
    )
  }

  md <- .mapdiff[freq == fr]

  # non heuristic conversion for non-heuristics
  if (nrow(md) == 0 ) { 
    z <- ts_to_POSIXct(x)
  
  # heuristic high freq > 12
  } else if (md$freq > 12) { 

    stopifnot(inherits(x, "ts"))
    if (NCOL(x) > 1) x <- x[, 1]
    start <- dectime_to_POSIXct(tsp(x)[1])
    # end <- dectime_to_POSIXct(tsp(x)[2])

      start <- round(start, "secs")

    # if (md$freq <= 525600){
    #   start <- round(start, "mins")
    #   # end <- round(end, "mins")
    # } else if (md$freq <= 8767){
    #   start <- round(start, "hours")
    #   # end <- round(end, "hours")
    # } else if (md$freq <= 365.25){
    #   # start <- round(start, "days")
    #   # end <- round(end, "days")
    # } else {
    #   start <- round(start, "secs")
    #   # end <- round(end, "secs")
    # }

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



date_time_to_tsp <- function(x) {
  if (length(x) <= 1) {
    stop("time series too short for frequency detection", call. = FALSE)
  }

  freq <- frequency_table(x)$freq
# browser()
  # Non heuristic conversion
  if (length(freq) > 1 || freq == -1){
    z <- POSIXct_to_tsp(as.POSIXct(x))
  # Low frequency conversion
  } else if (freq <= 12){
    st <- as.POSIXlt(x[1])
    y <- st$year + 1900L
    m <- st$mon + 1L
    d <- st$mday
    start <- y
    if (freq == 4) start <- c(y, ((m - 1) / 3) + 1)
    if (freq == 12) start <- c(y, m)
    if (d != 1) {
      stop("time column needs to specified as the first date of the period", call. = FALSE)
    }
    z <- tsp(ts(x, frequency = freq, start = start)) # a bit inefficient
  } else {
    reg.date <- regularize_date(x, full.year = TRUE)
    dum <- ts(reg.date, frequency = freq, start = data.table::year(x[1]))
    start <- time(dum)[dum >= as.integer(x[1])][1]
    z <- tsp(window(dum, start = start))
  }
  z
}
