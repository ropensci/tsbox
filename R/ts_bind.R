#' Bind Time Series
#'
#' Combine time series to a new, single time series. `ts_bind` combines time
#' series as they are, `ts_chain` chains them together, using percentage change
#' rates.
#'
#' @seealso [ts_c] to collect multiple time series
#' @param ... ts-boxable time series, objects of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#'
#' @return A ts-boxable object of the same class as the input.
#' If series of different classes are combined, the class of the first series is
#' used (if possible).
#'
#' @examples
#' ts_bind(ts_span(mdeaths, end = "1975-12-01"), fdeaths)
#' ts_bind(mdeaths, c(2, 2))
#' ts_bind(mdeaths, 3, ts_bind(fdeaths, c(99, 2)))
#' ts_bind(ts_dt(mdeaths), AirPassengers)
#' @export
ts_bind <- function(...) {
  ll <- list(...)

  tsboxable <- vapply(ll, ts_boxable, TRUE)
  desired.class <- desired_class(ll[tsboxable])

  # ll.dts <- lapply(ll, ts_dts)
  z <- Reduce(bind_two, ll)
  # setorder(z, time, var)

  as_class(desired.class)(z)
}


# Bind two dts objects
bind_two <- function(a, b) {
  value <- NULL
  value_b <- NULL

  a <- ts_dts(copy(a))

  cname <- dts_cname(a)

  # append scalars to dts object
  if (!ts_boxable(b)) {
    stopifnot(is.numeric(b))
    a <- ts_regular(a)

    add_scalar_one <- function(x) {
      diffdt <- frequency_table(x$time)
      # this is what we expect from a dts that went through ts_regular
      stopifnot(nrow(diffdt) == 1)
      stopifnot(diffdt$share == 1)

      new.x <- data.table(
        time = seq(from = max(x$time), length.out = length(b) + 1, by = diffdt$string)[-1],
        value = b
      )
      rbind(x, new.x)
    }

    setnames(a, cname$time, "time")
    setnames(a, cname$value, "value")
    .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
    z <- a[
      ,
      add_scalar_one(.SD),
      by = eval(.by)
    ]
    setnames(z, "value", cname$value)
    setnames(z, "time", cname$time)
    return(z)
  }

  b <- ts_dts(copy(b))

  setnames(a, cname$time, "time")
  setnames(b, dts_cname(b)$time, "time")

  setnames(a, cname$value, "value")
  setnames(b, cname$value, "value_b")

  if (!identical(cname$id, dts_cname(b)$id)) {
    stop(
      "Series do not have the same ids: ",
      paste(cname$id, collapse = ", "),
      "and",
      paste(dts_cname(b)$id, collapse = ", ")
    )
  }

  z <- merge(a, b, by = c(cname$id, "time"), all = TRUE)
  # remove key added by merge
  setkey(z, NULL)
  z <- z[is.na(value), value := value_b]
  z[, value_b := NULL]

  # canonical col order
  setcolorder(z, c(setdiff(names(z), c("time", "value")), c("time", "value")))

  setnames(z, "time", cname$time)
  setnames(z, "value", cname$value)
  z[]
}
