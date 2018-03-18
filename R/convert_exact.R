
# --- Convertors for dectime to POSIXct and back -------------------------------

seconds_since_70 <- function(year) {
  as.numeric(seq(
    as.POSIXct(paste0(year, "-01-01"), tz = ""), 
    length.out = 2, by = "1 year"
  ))
}

dectime_to_POSIXct <- function(x) {
  stopifnot(length(x) == 1)
  year <- floor(x)
  intra <- x - year
  ss70 <- seconds_since_70(year)
  # as current time zone
  as.POSIXct(ss70[1] + diff(ss70) * intra, origin = "1970-01-01", tz = "")
}

POSIXct_to_dectime <- function(x) {
  stopifnot(length(x) == 1)
  year <- as.POSIXlt(x)$year + 1900L
  ss70 <- seconds_since_70(year)

  intra <- (as.numeric(x) - ss70[1]) / diff(ss70)
  year + intra
}


# --- exact convertors ---------------------------------------------------------


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


POSIXct_to_tsp <- function(x) {
  stopifnot(inherits(x, "POSIXct"))
  dd <- diff(as.numeric(x))
  if ((max(dd) - min(dd)) > 1) {
    stop("some dates are not equally spaced.", call. = FALSE)
  }
  start <- POSIXct_to_dectime(x[1])
  end <- POSIXct_to_dectime(x[length(x)])
  f <- (length(x) - 1) / (end - start)
  c(start, end, f)
}
