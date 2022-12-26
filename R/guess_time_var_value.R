# to determine id, time, value, when coverting from data frame likes

# 3 times faster if we store .years
.years <- as.character(1600:2200)


#' Is This a time-like Vector?
#'
#' @param x any vector
#'
#' - Four digit integer are detected as years
#' - Use anytime() to detect time stamps
#'
#' @examples
#' is_time(ts_tbl(mdeaths)$time)
#' is_time(ts_tbl(mdeaths)$value)
#' @noRd
is_time <- function(x) {
  if (class(x)[1] %in% c("Date", "POSIXct")) {
    return(TRUE)
  } # beyond doubt
  # use a short vector for time detection
  if (length(x) > 20) {
    x <- c(
      x[1:3], # first 3
      x[(length(x) %/% 2 - 1):(length(x) %/% 2 + 1)], # middle 3
      x[(length(x) - 2):(length(x))]
    ) # lost 3
  }
  x <- as.character(x)

  # detect years as column
  if (all(x %in% .years)) {
    return(TRUE)
  }

  tt <- anytime(x, useR = TRUE)

  # useR = FALSE crashes R session on Win
  # https://github.com/eddelbuettel/anytime/issues/76
  if (any(is.na(tt))) {
    return(FALSE)
  }

  # exclude unrealistic years
  if ((as.POSIXlt(max(tt))$year + 1900L) > 2500) {
    return(FALSE)
  }

  TRUE
}


#' Is This a value-like Vector?
#'
#' @param x any vector
#'
#' - is.numeric() works for double and integer
#'
#' @examples
#' is_value(ts_tbl(mdeaths)$time)
#' @noRd
is_value <- function(x) {
  is.numeric(x) # also works for integer
}


#' Guess Time Column
#'
#' Using `is_time()`, this starts at the last column and determines the first
#' value column.
#'
#' @param x a data.frame
#'
#' @examples
#' guess_time(ts_tbl(mdeaths))
#' @noRd
guess_time <- function(x, value.name = "value") {
  stopifnot(inherits(x, "data.frame"))
  cnames <- colnames(x)
  if ("time" %in% cnames) {
    return("time")
  }

  cnames <- setdiff(cnames, value.name)

  z <- NA
  # start from the right column
  for (cname.i in rev(cnames)) {
    if (is_time(x[[cname.i]])) {
      z <- cname.i
      break
    }
  }

  if (is.na(z)) {
    stop0(
      "no [time] column detected; ",
      "to be explicit, name time column as 'time'"
    )
  }

  z
}


#' Guess Value Column
#'
#' Using `is_value()`, this starts at the last column and determines the first
#' value column.
#'
#' @param x a data.frame
#'
#' @examples
#' guess_value(ts_tbl(mdeaths))
#' @noRd
guess_value <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  cnames <- colnames(x)
  if ("value" %in% cnames) {
    return("value")
  }

  z <- NA
  for (cname.i in rev(cnames)) {
    if (is_value(x[[cname.i]])) {
      z <- cname.i
      break
    }
  }
  if (is.na(z)) {
    stop0(
      "no [value] column detected; ",
      "to be explicit, name value column as 'value'"
    )
  }
  z
}
