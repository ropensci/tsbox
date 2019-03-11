
#' @param denominator numeric, set equal to one if percentage change rate is given a
#'   decimal fraction
#' @name ts_index
#' @export
ts_compound <- function(x, denominator = 100) {
  not_in_data <- NULL
  value <- NULL
  z <- ts_dts(x)
  z <- ts_regular(ts_na_omit(z))

  cname <- dts_cname(z)
  setnames(z, cname$time, "time")
  setnames(z, cname$value, "value")

  z[, value := value / denominator + 1]

  # Adding a future value to get the right length of time series
  z <- ts_bind(ts_lag(z, -1), -99999)

  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))

  z[
    ,
    value := c(1, cumprod(value[-length(value)])),
    by = eval(.by)
  ]
  setnames(z, "value", cname$value)
  setnames(z, "time", cname$time)
  ts_na_omit(copy_class(z, x))
}



#' Indices from Levels or Percentage Rates
#'
#' `ts_index` returns an index series, with value of 1 at `base` date.
#' `ts_compound` builds an index from percentage change rates, starting with 1
#' and compounding the rates.
#'
#' @inherit ts_dts
#' @param base base date, character string, `Date` or `POSIXct`, at which the
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' head(ts_compound(ts_pc(ts_c(fdeaths, mdeaths))))
#' head(ts_index(ts_df(ts_c(fdeaths, mdeaths)), "1974-02-01"))
#' \donttest{
#' ts_plot(
#'   `My Expert Knowledge` = ts_chain(
#'     mdeaths,
#'     ts_compound(ts_bind(ts_pc(mdeaths), 15, 23, 33))),
#'   `So Far` = mdeaths,
#'   title = "A Very Manual Forecast"
#' )
#' }
#' @export
ts_index <- function(x, base = NULL) {

  not_in_data <- NULL
  value <- NULL
  z <- ts_dts(x)

  cname <- dts_cname(z)
  setnames(z, cname$time, "time")
  setnames(z, cname$value, "value")



  # if (inherits(z$time, "POSIXct")) {
  #   stop("indexing only works on 'Date', not 'POSIXct'")
  # }

  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))

  # use latest non na start point as base candidtate
  if (is.null(base)){
    dt_min_time <- z[
      !is.na(value),
      list(min.time = min(time)),
      by = eval(.by)
    ]
    base <- max(dt_min_time$min.time)
  } else {
    # let ts_span parse base and make sure it exists in data
    base <- range(ts_span(z, start = base)$time)[1]
  }

  # check if base date in data (rewrite)
  dt_in_data <- z[
    ,
    list(not_in_data = !(base %in% time)),
    by = eval(.by)
  ]
  if (any(dt_in_data$not_in_data)) {
    if (NCOL(z) > 3) {
      id.missing <- combine_cols_data.table(
        dt_in_data[not_in_data == TRUE], cname$id
      )$id
    } else if (NCOL(z) == 3) {
      id.missing <- dt_in_data[not_in_data == TRUE][[cname$id]]
    }

    if (length(cname$id) == 0) {
      stop(base, " not in series", call. = FALSE)
    } else {
      stop(
        base, " not in series: ",
        paste(id.missing, collapse = ", "),
        call. = FALSE
      )
    }
  }
  z[
    ,
    value := value / .SD[time == base, value],
    by = eval(.by)
  ]
  setnames(z, "value", cname$value)
  setnames(z, "time", cname$time)
  ts_na_omit(copy_class(z, x))
}
