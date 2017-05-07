
# x <- as.POSIXct(index(rbind(ts_xts(AirPassengers), ts_xts(mdeaths))))

# stopifnot(inherits(x, "POSIXct"))
# dd <- diff(as.integer(x))

# 3 * median(dd)


check_regularity <- function(x){

  stopifnot(inherits(x, "POSIXct"))
  dd <- diff(as.numeric(x))

  if ((max(dd) - min(dd)) > 1000){
    stop("some dates in xts are not equally spaced. Equality must be enforced, but the tools to do so still need to be implemented.")
  }
  if ((max(dd) - min(dd)) > 100){
    message("series seem not to be completely equally spaced, but may be still ok for ts conversion.")
  }
}


