
# library(tsbox)
# library(data.table)

# x <- bind_dts(lapply(paste0("var", 1:100), function(e) ts_dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "month"), var = e)))

#' @export
ts_ts <- function (x, ...) UseMethod("ts_ts")


#' @method ts_ts dts
ts_ts.dts <- function(x, ...) {
  wx <- spread_dts(x)
  tsp <- date_time_to_tsp(wx[, time])
  cdta <- wx[, -1]
  if (NCOL(cdta) == 1) {
    cdta <- as.numeric(cdta[[1]])
  } else {
    cdta <- as.matrix(cdta)
  }
  z <- ts(cdta, start = tsp[1], frequency = tsp[3])
  z
}


#' @method ts_dts ts
ts_dts.ts <- function(x, ...){
  stopifnot(inherits(x, "ts"))

  timec <- ts_to_date_time(x)

  dta <- data.table(x)

  dta[, time := timec]

  gather_dts(dta)
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
  ts_xts(ts_dts(x, ...))
}

#' @export
#' @method ts_data.frame ts
ts_data.frame.ts <- function(x, ...){
  ts_data.frame(ts_dts(x, ...))
}

#' @export
#' @method ts_data.table ts
ts_data.table.ts <- function(x, ...){
  ts_data.table(ts_dts(x, ...))
}









