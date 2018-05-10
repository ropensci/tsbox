#' Change Frequency
#'
#' Changes the frequency of a time series. By default, incomplete
#' periods of regular series are omitted.
#'
#' @inherit ts_dts
#' @param to desired frequency, either a character string (`"year"`,
#'  `"quarter"`, `"month"`) or an integer (`1`, `4`, `12`).
#' @param aggregate character string, or function. Either `"mean"`, `"sum"`,
#'  `"first"`, or `"last"`, or any aggregate function, such as [base::mean()].
#'  
#' @param na.rm logical, if `TRUE`, incomplete periods are aggregated as
#'   well. For irregular series, incomplete periods are always aggregated.
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
#' # Note that incomplete years are omited by default
#' ts_frequency(EuStockMarkets, "year")
#' ts_frequency(EuStockMarkets, "year", na.rm = TRUE)
#'
#' @export
ts_frequency <- function(x, to = "year", aggregate = "mean", na.rm = FALSE) {
  stopifnot(ts_boxable(x))
  z <- frequency_core(ts_dts(x), to = to, aggregate = aggregate, na.rm = na.rm)
  copy_class(z, x, preserve.mode = FALSE)
}

period.date <- list(
  month = date_month,
  quarter = date_quarter,
  year = date_year
)

numeric.period <- c(month = 12, quarter = 4, year = 1)

frequency_core <- function(x, to, aggregate, na.rm) {
  stopifnot(inherits(x, "dts"))

  # make sure incomplete periods result in NA
  if (na.rm == FALSE){
    try.x <- try(ts_regular(x))
    if (inherits(x, "try-error")){
      message("series is not regular, 'na.rm' set to TRUE. Aggregation may be based on incomplete periods")
      na.rm <- TRUE
    } else {
      x <- ts_bind(NA, try.x, NA)
    }
    
  }

  if (is.character(aggregate)) {
    if (!aggregate %in% c("mean", "sum", "first", "last")) {
      stop(
        "'aggregate' must be one of: 'mean', 'sum', 'first', 'last'",
        call. = FALSE
      )
    }
    aggregate <- switch(
      aggregate,
      mean = function(x) mean(x, na.rm = na.rm),
      sum = function(x) sum(x, na.rm = na.rm),
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

  cname <- dts_cname(x)

  if (length(cname$id) > 0) {
    byexpr <- parse(text = paste0(
      "list(",
      paste(cname$id, collapse = ", "), ", ",
      "time",
      ")"
    ))
  } else {
    byexpr <- expression(list(time))
  }

  x0 <- copy(x)
  data.table::setnames(x0, cname$value,  "value")
  data.table::setnames(x0, cname$time, "time")

  pdfun <- period.date[[to]]
  x0[, time := as.Date(pdfun(time))]

  z <- x0[, list(value = aggregate(value)), by = eval(byexpr)]
  z <- z[!is.na(value)]

  data.table::setnames(z, "value", cname$value)
  data.table::setnames(z, "time", cname$time)

  z[]
}
