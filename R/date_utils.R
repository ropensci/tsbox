#' Manipulating Dates
#'
#' `date_shift` adds seconds, minutes, hours, days, weeks, months, quarters or years to dates.
#' `date_month`, `date_quarter` and `date_yeae` return the first day of the
#' period and are useful for customized aggregation of data frames. For standard
#' aggregation, use [ts_frequency()].
#'
#' If `by` is character, the time stamp is shifted by a specific amount of time.
#' This can be one of one of `"sec"`, `"min"`, `"hour"`, `"day"`, `"week"`,
#' `"month"`, `"quarter" or `"year", optionally preceded by a (positive or
#' negative) integer and a space, or followed by plural "s". This is passed to
#' [base::seq.Date()]. This does not require the series to be regular.
#' 
#' @param x `Date` or `POSIXct`. If `POSIXct`, it is converted into `Date`.
#' @param by integer or character, either the number of shifting periods
#'   (integer), or an absolute amount of time (character). See details.
#' 
#' @return an object of class `Date`
#' @seealso [ts_frequency()] for standard aggregation. [date_shift()], for
#'   shifting time stamps of a ts-boxable object.
#'
#' @examples
#' ap.time <- ts_df(AirPassengers)$time
#' head(date_month(ap.time))
#' head(date_quarter(ap.time))
#' head(date_year(ap.time))
#'
#' head(date_shift(ap.time, 14))
#' head(date_shift(ap.time, "7 week"))
#' head(date_shift(ap.time, "-1 month"))
#'
#' date_shift(ts_end(mdeaths), 1)
#' date_shift(ts_end(mdeaths), "-14 sec")
#' date_shift(ts_end(mdeaths), "-1 year")
#' ts_span(ts_c(mdeaths, fdeaths), start = date_shift(ts_end(mdeaths), -1))
#' 
#' @export
date_shift <- function(x, by = NULL) {

  freq <- NULL
  
  if (is.null(by)) return(x)

  # high freq can be added to POSIXct only
  if (is.character(by) && grepl("sec|min|hour", by)){
    x <- as.POSIXct(x)
  }

  add_to_one_date <- function(x) seq(x, length.out = 2, by = by)[2]

  if (length(x) == 1) return(add_to_one_date(x))

  # this is the correct, perhaps not the fastest regularity test
  # also understands monthly, quarterly etc.
  diffdt <- frequency_table(x)
  fm <- diffdt[which.max(freq)]

  # if series is regular, take shortcut
  if (fm$share == 1) {
    if (is.numeric(by)){
      spl <- strsplit(diffdt$string, split = " ")[[1]]
      str <- paste(by * as.numeric(spl[1]), spl[2])
      add_to_one_date <- function(x) seq(x, length.out = 2, by = str)[2]
    }
    z <- seq(from = add_to_one_date(x[1]), by = fm$string, length.out = length(x))
    return(z)
  } 

  # if series is not regular, but seems regularizable, try and use shortcut if
  # possible
  if (fm$share < 1 && fm$share > 0.8){
    xreg <- regularize_date(x)
    if (!is.null(xreg)){
      if (is.numeric(by)){
        spl <- strsplit(diffdt$string, split = " ")[[1]]
        str <- paste(by * as.numeric(spl[1]), spl[2])
        add_to_one_date <- function(x) seq(x, length.out = 2, by = str)[2]
      }

      z <- seq(from = add_to_one_date(xreg[1]), by = fm$string, length.out = length(xreg))
      z <- z[match(as.integer(x), as.integer(xreg))]

      stopifnot(!any(is.na(z)))
      stopifnot(length(z) == length(x))

      return(z)
    }
  }
  
  if (is.numeric(by)) stop("by cannot be integer when used with irregular sequence", call. = FALSE)

  if (inherits(x, "Date")){
    # only do on unique values (which is faster in some cases)
    add_to_one_date <- function(x) seq(x, length.out = 2, by = by)[2]
    xu <- unique(x)
    zu <- do.call(c, lapply(xu, add_to_one_date))
    z <- merge(data.table(x = x), data.table(x = xu, z = zu), all.x = TRUE, sort = FALSE)$z
    return(z)
  }

  # shift each time stamp separately (slow)
  do.call(c, lapply(x, add_to_one_date))
}




#' @name date_shift
#' @export
date_month <- function(x) {
  x0 <- (x)
  d <- "1"
  m <- data.table::month(x0)
  y <- data.table::year(x0)
  z <- as.Date(paste(y, m, d, sep = "-"))
  if (inherits(x, "POSIXct")){
    z <- as.POSIXct(z, origin = "1970-01-01", tz = attr(x, "tzone"))
  }
  z
}

#' @name date_shift
#' @export
date_quarter <- function(x) {
  x0 <- (x)
  d <- "1"
  m <- (data.table::quarter(x) - 1) * 3 + 1
  y <- data.table::year(x0)
  z <- as.Date(paste(y, m, d, sep = "-"))
  if (inherits(x, "POSIXct")){
    z <- as.POSIXct(z, origin = "1970-01-01", tz = attr(x, "tzone"))
  }
  z
}

#' @name date_shift
#' @export
date_year <- function(x) {
  x0 <- (x)
  d <- "1"
  m <- "1"
  y <- data.table::year(x0)
  z <- as.Date(paste(y, m, d, sep = "-"))
  if (inherits(x, "POSIXct")){
    z <- as.POSIXct(z, origin = "1970-01-01", tz = attr(x, "tzone"))
  }
  z
}
