register_class("ts")

# to ---------------------------------------------------------------------------

ts_ts_dts <- function(x, frequency = NULL) {
  stopifnot(inherits(x, "dts"))

  wx <- wide_core(combine_id_cols(ts_regular(x)))

  if (is.null(frequency)){

    # try to regularize common time axis
    # this is needed if there are uncovered parts among several series
    reg.time <- regularize_date(wx[[1]])
    if (!is.null(reg.time)) {
      setnames(wx, 1, "time") # time col may have a different name
      wx <- merge_time_date(data.table::data.table(time = reg.time), wx, by.x = "time", by.y = "time")
    } else {
      stop("series has no regular pattern", call. = FALSE)
    }

    tsp <- date_time_to_tsp(wx[[1]])
    # if (inherits(tsp, "try-error")) {
    #   message(paste0(gsub("Error : |\\n", "", tsp), ", returning data.frame"))
    #   # browser()
    #   return(ts_df(x))
    # }
  } else {
    tsp <- date_time_to_tsp(wx[[1]], frequency = frequency)
  }
  # tsp <- date_time_to_tsp(wx[[1]])
  cdta <- wx[, -1]
  if (NCOL(cdta) == 1) {
    cdta <- as.numeric(cdta[[1]])
  } else {
    cdta <- as.matrix(cdta)
  }
  z <- ts(cdta, start = tsp[1], frequency = tsp[3])
  z
}


# from -------------------------------------------------------------------------

#' @export
#' @method ts_dts ts
ts_dts.ts <- function(x) {
  stopifnot(inherits(x, "ts"))
  timec <- ts_to_date_time(x)
  m <- as.matrix(x)
  dta <- data.table(m)
  dta[, time := timec]
  if (ncol(m) == 1) {
    names(dta)[1] <- "value"
    # needs the ts_dts.data.table
    setcolorder(dta, c("time", "value"))
  } else {
    dta <- melt(dta, id.vars = "time", variable.name = "id", variable.factor = FALSE)
    setcolorder(dta, c("id", "time", "value"))
  }
  # implicit NAs by default, use ts_regular to get explict NAs
  dts_init(dta)
}


# main converter ---------------------------------------------------------------

#' Convert Everything to Everything
#'
#' tsbox is built around a set of converters, which convert time series
#' stored as `ts`, `xts`, `data.frame`, `data.table` or `tibble` to each
#' other.
#'
#' In data frames, multiple time series will be stored in a 'long' format. tsbox
#' detects a *value*, a *time* and zero to several *id* columns. Column detection
#' is done in the following order:
#' 
#' 1. Starting **on the right**, the first first `numeric` or `integer` column is
#' used as **value column**.
#' 
#' 1. Using the remaining columns, and starting on the right again, the first
#' `Date`, `POSIXct`, `numeric` or `character` column is used as **time column**.
#' `character` strings are parsed by [anytime::anytime()]. 
#' The time stamp, `time`, indicates the beginning of a period. 
#' 
#' 1. **All remaining** columns are **id columns**. Each unique combination of id
#' columns points to a time series.
#'
#' **Alternatively**, the **time** column and the **value** column to be explicitly named as
#' `time` and `value`. If explicit names are used, the column order will be
#' ignored.
#'
#' Whenever possible, tsbox relies on **heuristic time conversion**. When a
#' monthly `"ts"` time series, e.g., `AirPassengers`, is converted to a data
#' frame, each time stamp (of class `"Date"`) is the first day of the month. In
#' most circumstances, this reflects the actual meaning of the data stored in a
#' `"ts"` object. Technically, of course, this is not correct: `"ts"` objects
#' divide time in period of equal length, while in reality, February is shorter
#' than January. Heuristic conversion is done for frequencies of 0.1 (decades),
#' 1 (years), 4 (quarters) and 12 (month).
#'
#' For other frequencies, e.g. 260, of `EuStockMarkets`, tsbox uses  **exact
#' time conversion**. The year is divided into 260 equally long units, and time
#' stamp of a period will be a point in time (of class `"POSIXct"`).
#'
#' @inherit ts_dts
#'
#' @return ts-boxable time series of the desired class, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#'
#' @examples
#'
#' x.ts <- ts_c(mdeaths, fdeaths)
#' head(x.ts)
#' head(ts_df(x.ts))
#' 
#' suppressMessages(library(dplyr))
#' head(ts_tbl(x.ts))
#' 
#' suppressMessages(library(data.table))
#' head(ts_dt(x.ts))
#' 
#' suppressMessages(library(xts))
#' head(ts_xts(x.ts))
#'
#' # heuristic time conversion
#' # 1 momth: approx. 1/12 year
#' head(ts_df(AirPassengers))
#'
#' # exact time conversion
#' # 1 trading day: exactly 1/260 year
#' head(ts_df(EuStockMarkets))
#'
#' # multiple id
#' multi.id.df <- rbind(
#'   within(ts_df(ts_c(fdeaths, mdeaths)), type <- "level"),
#'   within(ts_pc(ts_df(ts_c(fdeaths, mdeaths))), type <- "pc")
#' )
#' head(ts_ts(multi.id.df))
#' ts_plot(multi.id.df)
#'
#' @export
#' @import data.table
#' @importFrom anytime anydate anytime
#' @importFrom stats setNames as.ts frequency loess na.omit optimize predict resid time ts tsp as.formula var prcomp start tsp<- window
#' @importFrom utils getFromNamespace browseURL relist
#' @import data.table
ts_ts <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "ts") return(x)
  ts_ts_dts(ts_dts(x))
}
