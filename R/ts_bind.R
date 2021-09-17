#' Bind Time Series
#'
#' Combine time series to a new, single time series. `ts_bind` combines time
#' series as they are, `ts_chain` chains them together, using percentage change
#' rates.
#'
#' @seealso [ts_c] to collect multiple time series
#' @inherit ts_c
#'
#' @examples
#' ts_bind(ts_span(mdeaths, end = "1975-12-01"), fdeaths)
#' ts_bind(mdeaths, c(2, 2))
#' ts_bind(mdeaths, 3, ts_bind(fdeaths, c(99, 2)))
#' ts_bind(ts_dt(mdeaths), AirPassengers)
#'
#' # numeric vectors
#' ts_bind(12, AirPassengers, c(2, 3))
#' @export
ts_bind <- function(...) {
  ll <- list(...)

  tsboxable <- vapply(ll, ts_boxable, TRUE)
  desired.class <- desired_class(ll[tsboxable])

  z <- Reduce(bind_two, ll)

  as_class(desired.class)(z)
}


#' Bind 2 Time Series or Numeric Values
#'
#' Enables ts_bind() to work on scalars and vectors, too
#'
#' @param a ts-boxable object, or numeric, or one-dimensional input of any class
#' @param b ts-boxable object
#' @param backwards logical, should `b` be appended to `a`?
#'
#' @noRd
#' @srrstats {G2.6} *Software which accepts one-dimensional input should ensure values are appropriately pre-processed regardless of class structures.*
bind_numeric <- function(a, b, backwards = FALSE) {
  .SD <- NULL

  if (!ts_boxable(a)) {
    stop0("at least one object must be ts-boxable")
  }

  a <- ts_dts(copy(a))
  cname <- dts_cname(a)

  # allow logical NAs
  if (all(is.na(b)) && is.logical(b)) b <- as.numeric(b)

  stopifnot(is.numeric(b))
  a <- ts_regular(a)

  add_scalar_one <- function(x) {
    per.to.add <- length(b)

    if (!backwards) {
      # having at least 5 obs allows time_shift to detect frequency
      shft <- time_shift(
        x$time[max(length(x$time) - per.to.add - 5, 1):length(x$time)], per.to.add
      )
      new.time.stamps <- shft[(length(shft) - per.to.add + 1):length(shft)]
    } else {
      shft <- time_shift(x$time[1:min(per.to.add + 5, length(x$time))], -per.to.add)
      new.time.stamps <- shft[1:per.to.add]
    }

    new.x <- data.table(
      time = new.time.stamps,
      value = as.numeric(b)
    )

    z <- rbind(x, new.x)

    if (backwards) {
      setorder(z, time)
    }
    z
  }

  setnames(a, cname$time, "time")
  setnames(a, cname$value, "value")
  .by <- by_expr(cname$id)
  z <- a[
    ,
    add_scalar_one(.SD),
    by = eval(.by)
  ]
  setnames(z, "value", cname$value)
  setnames(z, "time", cname$time)

  return(z)
}


#' Bind 2 Time Series
#'
#' Successively called by ts_bind()
#'
#' @param a ts-boxable object
#' @param b ts-boxable object
#'
#' @noRd
bind_two <- function(a, b) {
  value <- NULL
  value_b <- NULL

  # append numeric to dts object
  if (!ts_boxable(b)) {
    return(bind_numeric(a, b))
  }
  if (!ts_boxable(a)) {
    return(bind_numeric(b, a, backwards = TRUE))
  }

  a <- ts_dts(copy(a))
  b <- ts_dts(copy(b))

  cols_a <- copy(names(a))

  default_colnames <- function(x) {
    cname <- attr(x, "cname")
    setnames(x, cname$time, "time")
    setnames(x, cname$value, "value")
    x
  }

  cname <- dts_cname(a)
  cname_b <- dts_cname(b)

  setnames(a, cname$time, "time")
  setnames(b, cname_b$time, "time")

  setnames(a, cname$value, "value")
  setnames(b, cname_b$value, "value_b")

  check_identical_ids(cname$id, dts_cname(b)$id)

  z <- merge(a, b, by = c(cname$id, "time"), all = TRUE)
  # remove key added by merge
  setkey(z, NULL)
  z <- z[is.na(value), value := value_b]
  z[, value_b := NULL]

  setnames(z, "time", cname$time)
  setnames(z, "value", cname$value)
  # keep order of first object
  setcolorder(z, cols_a)
  setattr(z, "cname", cname)
  dts_init_minimal(z)
}
