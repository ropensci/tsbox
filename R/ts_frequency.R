#' Change Frequency
#'
#' Changes the frequency of a time series. Currently, incomplete
#' periods are aggregated as well, but this is likely to change.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param to desired frequency, either a character string (`"year"`,
#'  `"quarter"`, `"month"`) or an integer (`1`, `4`, `12`).
#' @param aggregate character string, or function. Either `"mean"`, `"sum"`,
#'  `"first"`, or `"last"`, or any aggregate function, such as [base::mean()].
#'
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' ts_frequency(cbind(mdeaths, fdeaths), "year", "sum")
#' ts_frequency(cbind(mdeaths, fdeaths), "year", "sum")
#' ts_frequency(cbind(mdeaths, fdeaths), "quarter", "last")
#'
#' ts_frequency(AirPassengers, 4, "sum")
#' ts_frequency(AirPassengers, 1, "sum")
#'
#' # Note that incomplete years are (currently) aggregated as well
#' ts_frequency(EuStockMarkets, "year")
#'
#' @export
ts_frequency <- function(x, to = "year", aggregate = "mean") {
  stopifnot(ts_boxable(x))
  z <- frequency_core(ts_dts(x), to = to, aggregate = aggregate)
  copy_class(z, x)
}

period.date <- list(
  month = date_month,
  quarter = date_quarter,
  year = date_year
)

numeric.period <- c(month = 12, quarter = 4, year = 1)


frequency_core <- function(x, to, aggregate) {
  stopifnot(inherits(x, "dts"))

  if (is.character(aggregate)) {
    if (!aggregate %in% c("mean", "sum", "first", "last")) {
      stop(
        "'aggregate' must be one of: 'mean', 'sum', 'first', 'last'",
        call. = FALSE
      )
    }
    aggregate <- switch(
      aggregate,
      mean = function(x) mean(x, na.rm = TRUE),
      sum = function(x) sum(x, na.rm = TRUE),
      first = data.table::first,
      last = data.table::last
    )
  }

  if (!is.function(aggregate)) {
    stop(
      "'aggregate' must be of 'character' or 'function'",
      call. = FALSE
    )
  }

  value <- NULL

  if (is.numeric(to)) {
    stopifnot(to %in% numeric.period)
    to <- names(numeric.period)[numeric.period == to]
  }

  if (!(to %in% names(period.date))) {
    stop(
      "period", to, "not supported. Try one of: ",
      paste(names(period.date), collapse = ", ")
    )
  }

  if (length(colname_id(x)) > 0) {
    byexpr <- parse(text = paste0(
      "list(",
      paste(colname_id(x), collapse = ", "), ", ",
      "time",
      ")"
    ))
  } else {
    byexpr <- expression(list(time))
  }

  # temp renaming
  cvalue <- colname_value(x)
  ctime <- colname_time(x)

  x0 <- copy(x)
  data.table::setnames(x0, cvalue,  "value")
  data.table::setnames(x0, ctime, "time")

  pdfun <- period.date[[to]]
  x0[, time := pdfun(time)]

  z <- x0[, list(value = aggregate(value)), by = eval(byexpr)]
  data.table::setnames(z, "value", cvalue)
  data.table::setnames(z, "time", ctime)

  z[]
}
