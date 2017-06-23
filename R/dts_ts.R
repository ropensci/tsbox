
# library(tsbox)
# library(data.table)

# x <- bind_dts(lapply(paste0("var", 1:100), function(e) dts(rnorm(100), time = seq(as.Date("2001-01-01"), length.out = 100, by = "month"), var = e)))



#' @export
#' @method ts_ts dts
ts_ts.dts <- function(x, ...) {
  wx <- spread_dts(x)
  tsp <- Date_date_time_to_tsp(wx[, time])
  cdta <- wx[, -1]
  if (NCOL(cdta) == 1) cdta <- as.numeric(cdta)
  z <- ts(cdta, start = tsp[1], frequency = tsp[3])
  z
}


#' @export
#' @method dts ts
dts.ts <- function(x, ...){
  stopifnot(inherits(x, "ts"))

  timec <- ts_to_date_time

  dta <- as.data.table(x)

  dta[, time := timec]

  gather_dts(dta)
}


