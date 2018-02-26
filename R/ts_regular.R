#' Enforce Regularity
#'
#' Enforces regularity in data frame and `xts` objects, by turning implicit
#' `NA`s into explicit `NA`s. In `ts` objects, regularity is automatically
#' enforced.
#'
#' @param x a ts-boxable time series
#' @examples
#' x0 <- AirPassengers
#' x0[c(10, 15)] <- NA
#' x <- ts_na_omit(ts_dts(x0))
#' ts_regular(x)
#'
#' m <- mdeaths
#' m[c(10, 69)] <- NA
#' f <- fdeaths
#' f[c(1, 3, 15)] <- NA
#'
#' ts_regular(ts_na_omit(ts_dts(ts_c(f, m))))
#' @export
ts_regular <- function(x) {
  stopifnot(ts_boxable(x))
  z <- regular_core(ts_dts(x))
  copy_class(z, x)
}

regular_core <- function(x) {
  stopifnot(inherits(x, "dts"))
  ctime <- colname_time(x)
  cid <- colname_id(x)

  x <- copy(x)
  setnames(x, ctime, "time")

  # A quick regularity check to avoid full regualrization for most serise
  is_regular <- function(x) {
    if (any(is.na(x))) stop("time column cannot contain NAs", call. = FALSE)
    dd <- diff(as.numeric(x))
    rng <- max(dd) - min(dd)

    z <- (rng < 1) || (rng < 5 && rng / mean(dd) < 0.15)

    # if (!z && (max(dd) - min(dd) < 100)) message("missing speedup? at diff: ", max(dd) - min(dd))

    z
  }


  regular_core_one_series <- function(x) {
    # to speed it up
    if (is_regular(x$time)) return(x)

    reg.time <- regularize_date(x$time)

    if (is.null(reg.time)) {
      stop(
        "series does no show regular pattern and cannot be regularized",
        call. = FALSE
      )
    }
    # if POSIXct and successful regularization, change to date, to join
    if (inherits(reg.time, "Date") && inherits(x$time, "POSIXct")) {
      x$time <- as.Date(x$time)
    }
    merge(data.table(time = reg.time), x, by = "time", all.x = TRUE)
  }

  if (length(cid) == 0) {
    z <- regular_core_one_series(x)
  } else {
    z <- x[, regular_core_one_series(.SD), by = cid]
  }
  setnames(z, "time", ctime)
  z[]
}
