#' @export
#' @name ts_ts
#' @param lag defined as in dplyr, opposite to R base
#' @param fill how to fill missing values
ts_lag <- function(x, lag = 1, fill = NA){
  z <- ts_dts(x)

  if (lag < 0){
    type <- "lead"
    lag <- -lag
  } else {
    type <- "lag"
  }

  z[, value := shift(value, n = lag, fill = fill, type = type, give.names = FALSE), by = var]
  ts_reclass(z, x)
}

