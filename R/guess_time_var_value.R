# to determine id, time, value, when coverting from data frame likes

  # 3 times faster if we store .years
.years <- as.character(1600:2200)

is_time <- function(x) {
  if (class(x)[1] %in% c("Date", "POSIXct")) return(TRUE) # beyond doubt
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
  if (all(x %in% .years)) return(TRUE)

  tt <- anytime(x, useR = TRUE)

  # useR = FALSE crashes R session on Win
  # https://github.com/eddelbuettel/anytime/issues/76
  if (any(is.na(tt))) return(FALSE)

  # exclude unrealistic years
  if ((as.POSIXlt(max(tt))$year + 1900L) > 2500) return(FALSE)

  TRUE
}

is_value <- function(x) {
  is.numeric(x) # also works for integer
  # class(x)[1] %in% c("numeric")
}

guess_time <- function(x, value.name = "value") {
  stopifnot(inherits(x, "data.frame"))
  cnames <- colnames(x)
  if ("time" %in% cnames) return("time")

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
    stop("No [time] column detected. To be explict, name time column as 'time'.")
  }

  z
}

guess_value <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  cnames <- colnames(x)
  if ("value" %in% cnames) return("value")

  z <- NA
  for (cname.i in rev(cnames)) {
    if (is_value(x[[cname.i]])) {
      z <- cname.i
      break
    }
  }
  if (is.na(z)) {
    stop("No [value] column detected. To be explict, name value column as 'value'.")
  }
  z
}

