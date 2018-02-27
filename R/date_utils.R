#' Manipulating Dates
#'
#' `time_shift` adds seconds, minutes, hours, days, weeks, months, quarters or years to dates.
#' `first_day_of_month`, `first_day_of_quarter` and `first_day_of_yeae` return the first day of the
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
#' @seealso [ts_frequency()] for standard aggregation. [time_shift()], for
#'   shifting time stamps of a ts-boxable object.
#'
#' @examples
#' ap.time <- ts_df(AirPassengers)$time
#' head(first_day_of_month(ap.time))
#' head(first_day_of_quarter(ap.time))
#' head(first_day_of_year(ap.time))
#'
#' head(time_shift(ap.time, 14))
#' head(time_shift(ap.time, "7 week"))
#' head(time_shift(ap.time, "-1 month"))
#'
#' @export
time_shift <- function(x, by = NULL) {

  freq <- NULL
  
  if (is.null(by)) return(x)

  # high freq can be added to POSIXct only
  if (is.character(by) && grepl("sec|min|hour", by)){
    x <- as.POSIXct(x)
  }

  add_to_one_time <- function(x) seq(x, length.out = 2, by = by)[2]
  # this is the correct, perhaps not the fastest regularity test
  # also understands monthly, quarterly etc.
  diffdt <- frequency_table(x)
  fm <- diffdt[which.max(freq)]

  # if series is regular, take shortcut
  if (fm$share == 1) {
    if (is.numeric(by)){
      spl <- strsplit(diffdt$string, split = " ")[[1]]
      str <- paste(by * as.numeric(spl[1]), spl[2])
      add_to_one_time <- function(x) seq(x, length.out = 2, by = str)[2]
    }
    z <- seq(from = add_to_one_time(x[1]), by = fm$string, length.out = length(x))
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
        add_to_one_time <- function(x) seq(x, length.out = 2, by = str)[2]
      }

      z <- seq(from = add_to_one_time(xreg[1]), by = fm$string, length.out = length(xreg))
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
  do.call(c, lapply(x, add_to_one_time))
}




#' @name time_shift
#' @export
first_day_of_month <- function(x) {
  x <- as.Date(x)
  d <- "1"
  m <- data.table::month(x)
  y <- data.table::year(x)
  as.Date(paste(y, m, d, sep = "-"))
}

#' @name time_shift
#' @export
first_day_of_quarter <- function(x) {
  x <- as.Date(x)
  d <- "1"
  m <- (data.table::quarter(x) - 1) * 3 + 1
  y <- data.table::year(x)
  as.Date(paste(y, m, d, sep = "-"))
}

#' @name time_shift
#' @export
first_day_of_year <- function(x) {
  x <- as.Date(x)
  d <- "1"
  m <- "1"
  y <- data.table::year(x)
  as.Date(paste(y, m, d, sep = "-"))
}
