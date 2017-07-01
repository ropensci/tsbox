
# library(tsbox)
# library(data.table)

# x <- bind_dts(lapply(paste0("var", 1:100), function(e) ts_dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "month"), var = e)))

#' Convert everything to everything
#' 
#' @param x a time series object, either `ts`, `data.frame`, `data.table`, `tibble` or `xts`.
#' @param ... additional arguments, passed to methods
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
#' @importFrom utils browseURL
#' @import data.table 
ts_ts <- function (x, ...) UseMethod("ts_ts")




#' @export
#' @name ts_ts
#' @method ts_ts dts
ts_ts.dts <- function(x, ...) {
  x <- ts_combine(x)
  wx <- spread_core(x)
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

#' @export
#' @method ts_dts ts
ts_dts.ts <- function(x, name = NULL, ...){

  stopifnot(inherits(x, "ts"))

  timec <- ts_to_date_time(x)
  m <- as.matrix(x)
  if (NCOL(m) == 1) {
    if (is.null(name)){
      colnames(m) <- deparse(substitute(x))
    } else {
      colnames(m) <- name
    }
  }
  dta <- data.table(m)
  dta[, time := timec]
  gather_core(dta)
}





# --- all methods --------------------------------------------------------------

#' @export
#' @method ts_ts ts
ts_ts.ts <- function(x, ...){
  x
}

#' @export
#' @method ts_xts ts
ts_xts.ts <- function(x, ...){
  ts_xts(ts_dts(x, name = deparse(substitute(x)), ...))
}

#' @export
#' @method ts_data.frame ts
ts_data.frame.ts <- function(x, ...){
  ts_data.frame(ts_dts(x, name = deparse(substitute(x)), ...))
}

#' @export
#' @method ts_data.table ts
ts_data.table.ts <- function(x, ...){
  ts_data.table(ts_dts(x, name = deparse(substitute(x)), ...))
}









