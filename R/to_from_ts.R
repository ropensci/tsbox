# to ---------------------------------------------------------------------------

ts_ts_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  wx <- spread_core(combine_id_cols(x))
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
ts_dts.ts <- function(x){
  stopifnot(inherits(x, "ts"))
  timec <- ts_to_date_time(x)
  m <- as.matrix(x)
  dta <- data.table(m)
  dta[, time := timec]
  if (ncol(m) == 1){
    names(dta)[1] <- "value"
    # needs the ts_dts.data.table
    z <- ts_dts(dta[, .(time, value)])

  } else {
    z <- gather_core(dta)
  }
  z
}


# main converter ---------------------------------------------------------------

# could be probably automated

#' Convert everything to everything
#' 
#' @param x a time series object, either `ts`, `data.frame`, `data.table`, `tibble` or `xts`.
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
#' @importFrom stats as.ts frequency loess na.omit optimize predict resid time ts tsp as.formula var
#' @importFrom utils browseURL relist
#' @import data.table 
ts_ts <- function(x){
  stopifnot(ts_boxable(x))
  if (relevant_class(x) == "ts")  return(x)
  ts_ts_dts(ts_dts(x))
}

