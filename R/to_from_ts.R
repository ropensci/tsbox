# to ---------------------------------------------------------------------------

ts_ts_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  x <- ts_regular(x)
  wx <- wide_core(combine_id_cols(x))

  # try to regularize common time axis
  reg.time <- regularize_date(wx[[1]])
  if (!is.null(reg.time)){
    setnames(wx, 1, "time") # time col may have a different name
    if (inherits(wx[[1]], "POSIXct")) wx[[1]] <- as.Date(wx[[1]])
    wx <- wx[data.table(time = reg.time), on = "time"]
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
    z <- ts_na_omit(z)
  }
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
#' **or** the **time** colum and the **value** column to be explicitly named as `time` and `value`. If explicit names are used, the column order will be ignored.
#' 
#' Note that multiple id columns with arbitrary names are allowed.
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
