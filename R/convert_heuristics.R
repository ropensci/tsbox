

ts_to_date_time <- function(x){
  stopifnot(inherits(x, "ts"))
  if (NCOL(x) > 1) x <- x[,1]

  # basic strucuture from the xts package

  # But: 
  # - assuming time is measured in years, which makes conversion easier

  # Anything we can do wiht a weekly series? 

  # e.g. ts(rnorm(50), start = 1, f = 7)?  
  # Probably not. So this would currently be read as a a regular
  # 1/7 period, starting in year 1. Seems ok to me.

  # - TODO more frequencies

  # # heuristic converions
  if(frequency(x) == 1) {

    # perhaps we can make this more general: if subperiod is hole number, 
    # threat it as, month, quarter, semester
    y <- tsp(x)[1] %/% 1
    m <- tsp(x)[1] %%  1
    if (m %% (1/12) == 0) {
      m <- ifelse(length(m) < 1, 1, floor(m * 12) + 1)
      from <- as.Date(ISOdate(year = y, month = m, day = 1))
      z <- seq.Date(from, length.out = length(x), by = "1 year")
    }
  } else if(frequency(x) == 4) {
    z <- as.Date(as.yearqtr(time(x)))
  } else if(frequency(x) == 12) {
    z <- as.Date(as.yearmon(time(x)))
  } else {
    # non heuristic conversion
    z <- ts_to_POSIXct(x)
  }

  z
}

# not too bad how this works for most series

# ts_to_date_time(AirPassengers)
# ts_to_date_time(uspop)
# ts_to_date_time(austres)
# ts_to_date_time(EuStockMarkets)



# # utility function to find POSIXct range (for coding only)
# find_range <- function(x){
#   ser <- ts(rep(1, 1000), f = x, start = 1800)
#   range(diff(as.numeric(as.POSIXct(ts_to_date_time(ser)))))
# }

# find_range(12)



in_range <- function(x, min, max, tol = 1000){
  (all(x < (max + tol)) & all(x > (min - tol)))
}


date_time_to_tsp <- function(x){ 

  st <- as.POSIXlt(x[1])
  y <- st$year + 1900L
  m <- st$mon + 1L
  d <- st$mday

  ds <- range(diff(as.numeric(as.POSIXct(x))))
  
  if (in_range(ds, 31536000, 31622400)){
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
    z <- date_time_to_tsp(as.POSIXct(x))
  }
  z
}
