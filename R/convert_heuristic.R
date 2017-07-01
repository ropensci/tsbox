

ts_to_date_time <- function(x){
  stopifnot(inherits(x, "ts"))

  # if 'mts', only consider first column
  if (NCOL(x) > 1) x <- x[,1]

  first.year <- tsp(x)[1] %/% 1
  first.subperiod <- tsp(x)[1] %%  1
  fr <- frequency(x)

  if (abs(first.subperiod %% (1 / fr)) > 1e-8){
    stop("Suberiod is not dividable by fr. Could be also a rounding problem.")
  }

  # make more general?
  by.string <- switch(as.character(fr), 
                      `0.1`      = "10 year",
                      `1`        = "1 year",
                      `2`        = "6 month",
                      `4`        = "1 quarter",
                      `12`       = "1 month"
                      # `365.25`   = "1 day"   
                      )

  if (is.null(by.string)){   # non heuristic conversion as fall back
    return(ts_to_POSIXct(x))
  }

  month.per.unit <- 12 / fr
  first.month <- round((first.subperiod * fr) * month.per.unit + 1)
  first.Date <- as.Date(ISOdate(year = first.year, 
                                month = first.month,
                                day = 1))
  stopifnot(!is.na(first.Date))

  seq.Date(first.Date, length.out = length(x), by = by.string)
}

# not too bad how this works for most series

# ts_to_date_time(AirPassengers)
# ts_to_date_time(uspop)
# ts_to_date_time(austres)
# ts_to_date_time(EuStockMarkets)



# utility function to find POSIXct range (for coding only)
find_range <- function(x){
  ser <- ts(rep(1, 1000), f = x, start = 1800)
  range(diff(as.numeric(as.POSIXct(ts_to_date_time(ser)))))
}

# find_range(0.1)



in_range <- function(x, min, max, tol = 1000){
  (all(x < (max + tol)) & all(x > (min - tol)))
}


date_time_to_tsp <- function(x){ 

  if (length(x) <= 1) {
    stop("Time series too short for frequency detection.", call. = FALSE)
  }

  st <- as.POSIXlt(x[1])
  y <- st$year + 1900L
  m <- st$mon + 1L
  d <- st$mday

  ds <- range(diff(as.numeric(as.POSIXct(x))))

  # TODO Write more efficiently, and use unified lookup table 
  # (same as in ts_to_date_time())
  if (in_range(ds, 315532800, 315619200)) {
    f <- 0.1
    start <- y
  } else if (in_range(ds, 31536000, 31622400)){
    f <- 1
    start <- y
    # start <- y + (1 / (m - 1))  
  } else if (in_range(ds, 7776000, 7948800)){
    f <- 4
    if (!(m %in% (c(1, 4, 7, 10)))) { 
      stop("Quarterly data needs to specified as start of period (currently)")
    }
    # 3*((1:4)-1)+1   ## oposite
    start <- c(y, ((m - 1) / 3) + 1)
  } else if (in_range(ds, 2419200, 2678400)){
    f <- 12
    start <- c(y, m)
  } else {
    f <- NULL
  }

  if (!is.null(f)) {
    if (d != 1){
      stop("Data needs to specified as start of period (currently)")
    }
    z <- tsp(ts(x, frequency = f, start = start))  # a bit inefficient
  } else {
    # non heuristic conversion
    z <- POSIXct_to_tsp(as.POSIXct(x))
  }
  z
}
