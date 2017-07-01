
# x <- as.POSIXct(index(rbind(ts_xts(AirPassengers), ts_xts(mdeaths))))

# stopifnot(inherits(x, "POSIXct"))
# dd <- diff(as.integer(x))

# 3 * median(dd)


check_regularity <- function(x){

  stopifnot(inherits(x, "POSIXct"))
  dd <- diff(as.numeric(x))

  if ((max(dd) - min(dd)) > 1000){
    stop("Some dates are not equally spaced. \n\nEquality should be enforced, but the tools are not yet implemented.", call. = FALSE)
  }
  if ((max(dd) - min(dd)) > 100){
    message("series seem not to be completely equally spaced, but may be still ok for ts conversion.")
  }
}


