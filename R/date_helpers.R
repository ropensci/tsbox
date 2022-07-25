#' Convert Anything to Date or POSIXct
#'
#' @param x Date, POSIXct, or anything that can be coerced to character
#' @examples
#' as_time_or_date("2021.3")
#' @noRd
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


#' Regularize Dates
#'
#' If `x` is regular, it is as fast as possilbe, and checks reliably for
#' regularity.
#'
#' @param x Date or POSIXct
#' @noRd
regularize_date <- function(x) {
  stopifnot(inherits(x, c("POSIXct", "Date")))

  N <- NULL
  freq <- NULL
  share <- NULL
  string <- NULL

  x <- sort(x)

  diffdt <- frequency_table(x)
  fm <- diffdt[which.max(freq)]

  if (is_near(fm$freq, -1)) {
    return(regularize_non_heuristic(x))
  }

  # regular, exit
  if (is_near(fm$share, 1)) {
    return(x)
  }

  from <- x[1]
  to <- x[length(x)]

  if (inherits(x, "POSIXct")) {

    # for some reason, POSIXct is not precise for quartals
    if (fm$freq <= 12 && fm$freq > -1) {
      z <- as.POSIXct(
        seq(from = as.Date(from), to = as.Date(to), by = fm$string),
        tz = attr(x, "tzone")
      )
      if (!all(as.integer(x) %in% as.integer(z))) {
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
  if (!all(as.integer(x) %in% as.integer(z))) {
    return(NULL)
  }
  z
}

#' Regularize Dates without Heuristics
#'
#' If heuristics don't work, this slow routine used for Date regularization
#'
#' @param x Date or POSIXct
#' @noRd
regularize_non_heuristic <- function(x) {
  stopifnot(inherits(x, c("POSIXct", "Date")))

  x.num <- as.numeric(x)
  dd <- unique(round(diff(x.num), 5))

  if (length(dd) == 1L) {
    return(x)
  } # already regular

  min.dd <- min(dd)

  # all diffs must be integer multiples of minimal diff
  if (any((dd %% min.dd) > 0.1)) {
    return(NULL)
  }

  sq <- seq(from = x.num[1], to = x.num[length(x.num)] + 0.1, by = min.dd)

  if (inherits(x, "POSIXct")) {
    z <- as.POSIXct(sq, origin = "1970-01-01", tz = attr(x, "tzone"))
  } else {
    z <- as.Date(sq, origin = "1970-01-01", tz = attr(x, "tzone"))
  }

  dtx <- data.table(x, s = seq_along(x), x0 = x)
  dtz <- data.table(x = z + 0.1, z0 = z)
  rj <- dtx[dtz, roll = 1, on = "x"]
  if (!all(dtx$s %in% rj$s)) {
    return(NULL)
  }
  rj$z0
}
