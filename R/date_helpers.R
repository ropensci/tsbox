as_time_or_date <- function(x){
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  # We want to return a date unless its really a time
  anydate(as.character(x))
}

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
