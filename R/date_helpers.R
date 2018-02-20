as_time_or_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  # We want to return a date unless its really a time
  anydate(as.character(x))
}

check_regularity <- function(x) {
  stopifnot(inherits(x, "POSIXct"))
  dd <- diff(as.numeric(x))
  if ((max(dd) - min(dd)) > 1000) {
    stop("some dates are not equally spaced.", call. = FALSE)
  }
}


regularize_date <- function(x, full.year = FALSE) {
  stopifnot(class(x)[1] %in% c("POSIXct", "Date"))

  N <- NULL
  freq <- NULL
  share <- NULL
  string <- NULL

  x <- sort(x)

  diffdt <- frequency_table(x)

  if (nrow(diffdt[string == ""]) > 0 && diffdt[string == "", share] > 0.5) {
    # return NULL if regularization failed
    return(NULL)
  }
  fm <- diffdt[which.max(freq)]
# browser()
  if (is.na(fm$string) && fm$share == 1) fm$string <- unique(diff(as.numeric(x)))[1]

  # No POSIXct time stamps for low freq series
  if (fm$freq <= 12 && inherits(x, "POSIXct")){
    x <- as.Date(x)
  }

  from <- x[1]
  to <- x[length(x)]

  if (full.year){
    from <- date_year(from)
    if (inherits(x, "POSIXct")){
      from <- as.POSIXct(from)
    }
    # to <- as.POSIXct(date_shift(date_year(from), "1 year"))-1
  }
  if (inherits(x, "POSIXct")){
    z <- seq(from = from, to = to + 1, by = fm$string)
  } else {
    z <- seq(from = from, to = to, by = fm$string)
  }
  
  # return NULL if regularization failed
  if (!all(as.integer(x) %in% as.integer(z))) return(NULL)
  z
}
