
# --- Convertors for dectime to POSIXct and back -------------------------------

seconds_since_70 <- function(year){
  sq <- seq(as.POSIXct("1990-01-01", tz = ""), 
            to = as.POSIXct("2010-01-01", tz = ""), 
            by = "1 year")
  as.numeric(seq(as.POSIXct(paste0(year, "-01-01")), length.out = 2, by = "1 year"))
}

dectime_to_POSIXct <- function(x){
  stopifnot(length(x) == 1)
  year <- floor(x)
  intra <- x-year
  ss70 <- seconds_since_70(year)
  as.POSIXct(ss70[1] + diff(ss70) * intra, origin = "1970-01-01", tz = "")
}

POSIXct_to_dectime <- function(x){
  stopifnot(length(x) == 1)
  year <- as.POSIXlt(x)$year + 1900L
  ss70 <- seconds_since_70(year)

  intra <- (as.numeric(x) - ss70[1]) / diff(ss70)
  year + intra
}


# Extensive test

# sq <- seq(as.POSIXct("0001-01-01", tz = ""), 
#           to = as.POSIXct("2500-01-01", tz = ""), 
#           by = "1 hour")
# random.dates <- sample(sq, 1000)
# 
# z <- vapply(random.dates, function(e) all.equal(dectime_to_POSIXct(POSIXct_to_dectime(# x)),x), TRUE)
# all(z)  # TRUE


# --- main convertors ----------------------------------------------------------


ts_to_POSIXct <- function(x){
  stopifnot(inherits(x, "ts"))
  if (NCOL(x) > 1) x <- x[,1]
  seq(from = dectime_to_POSIXct(tsp(x)[1]),
      to = dectime_to_POSIXct(tsp(x)[2]),
      length.out = length(x))
}

POSIXct_to_tsp <- function(x){
  stopifnot(inherits(x, "POSIXct"))
  start <- POSIXct_to_dectime(x[1])
  end <- POSIXct_to_dectime(x[length(x)])
  f <- (length(x) - 1) / (end - start)
  c(start, end, f)
}

# all.equal(POSIXct_to_tsp(ts_to_POSIXct(AirPassengers)), tsp(AirPassengers))
# all.equal(POSIXct_to_tsp(ts_to_POSIXct(EuStockMarkets)), tsp(EuStockMarkets))
# all.equal(POSIXct_to_tsp(ts_to_POSIXct(discoveries)), tsp(discoveries))
# all.equal(POSIXct_to_tsp(ts_to_POSIXct(mdeaths)), tsp(mdeaths))
# all.equal(POSIXct_to_tsp(ts_to_POSIXct(uspop)), tsp(uspop))
# all.equal(POSIXct_to_tsp(ts_to_POSIXct(austres)), tsp(austres))





