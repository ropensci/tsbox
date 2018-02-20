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

  # # No POSIXct time stamps for low freq series
  # if (fm$freq <= 12 && inherits(x, "POSIXct")){
  #   x <- as.Date(x)
  # }
# browser()
  from <- x[1]
  to <- x[length(x)]
  if (full.year){
    if (inherits(x, "POSIXct")){
      from <- ISOdate(data.table::year(round(from, "mins")), 1, 1, hour = 0, tz = attr(x, "tzone"))
    } else {
      from <- date_year(from)
    }
    # to <- as.POSIXct(date_shift(date_year(from), "1 year"))-1
  }
  if (inherits(x, "POSIXct")){

    # for some reason, POSIXct are not precice for quartals
    if (fm$freq <= 12){
      z <- as.POSIXct(seq(from = as.Date(from), to = as.Date(to), by = fm$string), tz = attr(x, "tzone"))
      if (!all(as.integer(x) %in% as.integer(z))){
        # but sometimes it does, second try
        z <- seq(from = from, to = to + 0.1, by = fm$string)
      }
    } else {
      z <- seq(from = from, to = to + 0.1, by = fm$string)
    }
  } else {
    z <- seq(from = from, to = to, by = fm$string)
  }
  
  # browser()
  # return NULL if regularization failed
  if (!all(as.integer(x) %in% as.integer(z))) return(NULL)
  z
}
