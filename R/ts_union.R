#' Align Time Series
#' 
#' `ts_align` makes sure all time series cover the same time span and have the
#' same regular frequency. `ts_union` only ensures that series have the same
#' time stamps and also works with irregular series. For `ts` objects, the two 
#' functions have the same effect.
#' 
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param fill missign value specifier
#' @return a ts-boxable time series, with the same class as the input.
#' 
#' @export
#' @examples
#' ts_align(ts_df(ts_c(mdeaths, fdeaths = ts_window(fdeaths, end = "1977-01-01"))))
ts_align <- function(x, fill = NA){
  stopifnot(length(fill) == 1)
  x0 <- ts_ts(x)
  # copy pasted from ts_dts.ts
  value <- NULL
  timec <- ts_to_date_time(x0)
  m <- as.matrix(x0)
  dta <- data.table(m)
  dta[, time := timec]
  if (ncol(m) == 1) {
    names(dta)[1] <- "value"
    # needs the ts_dts.data.table
    z <- ts_dts(dta[, list(time, value)])
  } else {
    z <- long_core(dta)
    if (!is.na(fill)){
      z[is.na(value), value := fill]
    }
  }
  copy_ts_class(z, x)
}


#' @name ts_align
#' @export
ts_union <- function(x, fill = NA) {
  stopifnot(length(fill) == 1)

  k <- NULL
  value <- NULL

  x1 <- ts_dts(x)

  if (number_of_series(x1) == 1) return(x)

  # colname.value <- colname_value(x1)
  colname.time <- colname_time(x1)
  colname.id <- colname_id(x1)

  full.time <- unique(x1[, colname.time, with = FALSE])

  # all vars in the data
  full.var <- unique(x1[, colname.id, with = FALSE])
  full.var[, k := 1] # dummy merge variable
  full.time[, k := 1]

  full.frame <- merge(full.var, full.time, by = "k", allow.cartesian = TRUE)
  full.frame[, k := NULL]

  z <- merge(full.frame, x1, by = colnames(full.frame), all.x = TRUE)

  # this also overwrites existing NAs
  if (!is.na(fill)) {
    z[is.na(value), value := fill]
  }

  copy_ts_class(z, x)
}
