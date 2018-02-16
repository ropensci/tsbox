# to ---------------------------------------------------------------------------

ts_ts_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  wx <- wide_core(combine_id_cols(ts_regular(x)))

  # try to regularize common time axis
  reg.time <- regularize_date(wx[[1]])
  if (!is.null(reg.time)){
    setnames(wx, 1, "time") # time col may have a different name
    if (inherits(wx[[1]], "POSIXct")) wx[[1]] <- as.Date(wx[[1]])
    wx <- wx[data.table(time = reg.time), on = "time"]
  }

  tsp <- try(date_time_to_tsp(wx[[1]]), silent = TRUE)
  if (inherits(tsp, "try-error")){
    message(paste0(gsub("Error : |\\n", "", tsp), ", returning data.frame"))
    # browser()
    return(ts_df(x))
  }
  tsp <- date_time_to_tsp(wx[[1]])
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
  value <- NULL
  stopifnot(inherits(x, "ts"))
  timec <- ts_to_date_time(x)
  m <- as.matrix(x)
  dta <- data.table(m)
  dta[, time := timec]
  if (ncol(m) == 1) {
    names(dta)[1] <- "value"
    # needs the ts_dts.data.table
    z <- ts_dts(dta[, list(time, value)])
  } else {
    z <- long_core(dta)
  }
  # implicit NAs by default, use ts_regular to get explict NAs
  z <- ts_na_omit(z)
  z
}


# main converter ---------------------------------------------------------------

#' Convert Everything to Everything
#' 
#' tsbox is built around a set of converters, which convert time series
#' stored as `ts`, `xts`, `data.frame`, `data.table` or `tibble` to each
#' other. 
#'  
#' Mulitple time series will be stored as a 'long' data frame (`data.frame`,
#' `data.table` or `tibble`):
#' 
#' ```
#' ts_df(ts_c(fdeaths, mdeaths))
#' 
#' #          id       time value
#' # 1   fdeaths 1974-01-01   901
#' # 2   fdeaths 1974-02-01   689
#' # 3   fdeaths 1974-03-01   827
#' # 4   fdeaths 1974-04-01   677
#' # 5   fdeaths 1974-05-01   522
#' # ...
#' # 140 mdeaths 1979-08-01   975
#' # 141 mdeaths 1979-09-01   940
#' # 142 mdeaths 1979-10-01  1081
#' # 143 mdeaths 1979-11-01  1294
#' # 144 mdeaths 1979-12-01  1341
#' ```
#' 
#' The time stamp, `time`, indicates the beginning of a period. tsbox requires the
#' columns in a data frame to follow either the order:
#' 
#' 1. **id** column(s)
#' 2. **time** column
#' 3. **value** column
#' 
#' **or** the **time** column and the **value** column to be explicitly named as `time` and `value`. If explicit names are used, the column order will be ignored.
#' 
#' Note that multiple id columns with arbitrary names are allowed.
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
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' 
#' @return ts-boxable time series of the desired class, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' 
#' @examples
#'
#' x.ts <- ts_c(mdeaths, fdeaths)
#' x.df <- ts_df(x.ts)
#' x.dt <- ts_dt(x.df)
#' 
#' # heuristic time conversion
#' ts_df(AirPassengers)
#' 
#' # exact time conversion
#' ts_df(EuStockMarkets)
#' 
#' # Multiple IDs
#' multi.id.df <- rbind(
#'   within(ts_df(ts_c(fdeaths, mdeaths)), type <- "level"),
#'   within(ts_pc(ts_df(ts_c(fdeaths, mdeaths))), type <- "pc")
#' )
#' ts_plot(multi.id.df)
#' ts_ts(multi.id.df)
#' 
#' \dontrun{
#' library(xts)
#' x.xts <- ts_xts(x.ts)
#' library(dplyr)
#' x.tbl <- ts_tbl(x.ts)
#' }
#'
#' @export
#' @import data.table
#' @importFrom anytime anydate anytime
#' @importFrom stats as.ts frequency loess na.omit optimize predict resid time ts tsp as.formula var prcomp start tsp<-
#' @importFrom utils browseURL relist
#' @import data.table
ts_ts <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "ts") return(x)
  ts_ts_dts(ts_dts(x))
}
