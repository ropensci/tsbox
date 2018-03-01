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

# if this is regular, it is as fast as possilbe, and checks reliably for 
# regularity.
regularize_date <- function(x) {
  stopifnot(class(x)[1] %in% c("POSIXct", "Date"))

  N <- NULL
  freq <- NULL
  share <- NULL
  string <- NULL

  x <- sort(x)

  diffdt <- frequency_table(x)
  fm <- diffdt[which.max(freq)]
  
  # standard freq or not, if there is a single difference, it is already
  # regular, exit
  if (fm$share == 1) return(x)

  from <- x[1]
  to <- x[length(x)]

  if (inherits(x, "POSIXct")){

    # for some reason, POSIXct is not precise for quartals
    if (fm$freq <= 12){
      z <- as.POSIXct(seq(from = as.Date(from), to = as.Date(to), by = fm$string), tz = attr(x, "tzone"))
      if (!all(as.integer(x) %in% as.integer(z))){
        # but sometimes it is, so give it a second try
        z <- seq(from = from, to = to + 0.1, by = fm$string)
      }
    } else {
      z <- seq(from = from, to = to + 0.1, by = fm$string)
    }
  } else {
    z <- seq(from = from, to = to, by = fm$string)
  }

  # return NULL if regularization failed
  if (!all(as.integer(x) %in% as.integer(z))) return(NULL)
  z
}


# same as above, but returns integer, does not change between POSIXct and Date 
regularize_date_int <- function(x) {
  stopifnot(class(x)[1] %in% c("POSIXct", "Date"))

  N <- NULL
  freq <- NULL
  share <- NULL
  string <- NULL

  x <- sort(x)

  diffdt <- frequency_table(x)
  fm <- diffdt[which.max(freq)]
  
  # standard freq or not, if there is a single difference, it is already
  # regular, exit
  if (fm$share == 1) return(x)

  from <- x[1]
  to <- x[length(x)]

  if (inherits(x, "POSIXct")){

    # for some reason, POSIXct is not precise for quartals
    if (fm$freq <= 12){
      z <- as.POSIXct(seq(from = as.Date(from), to = as.Date(to), by = fm$string), tz = attr(x, "tzone"))
      if (!all(as.integer(x) %in% as.integer(z))){
        # but sometimes it is, so give it a second try
        z <- seq(from = from, to = to + 0.1, by = fm$string)
      }
    } else {
      z <- seq(from = from, to = to + 0.1, by = fm$string)
    }
  } else {
    z <- seq(from = from, to = to, by = fm$string)
  }

  stopifnot(identical(class(x), class(z)))

  z <- as.integer(z)
  # return NULL if regularization failed
  if (!all(as.integer(x) %in% z)) return(NULL)
  z
}
