


#' Combine or separate variable names in data.frame like objects
#' @param x any time series object
#' @param into new column names
#' @export
ts_combine <- function(x){
  x.dts <- ts_dts(x)
  if (NCOL(x.dts) <= 3) return(x)
  var.names <- colnames(x.dts)[-c(1, 2)]
  z <- combine_cols_data.table(x.dts, var.names)
  ts_reclass(z, x)
}


#' @export
#' @name ts_combine
ts_separate <- function(x, into){
  # ... do not allow for ts, xts
  z <- ts_data.table(x)
  z[, (into) := tstrsplit(var, "_", fixed=TRUE)]
  z[, var := NULL]
  ts_reclass(z, x)
}


