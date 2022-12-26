#' Default Column Names
#'
#' In data frame objects (`data.frame`, `tibble`, `data.table`), tsbox
#' automatically detects the time and the value column. This function changes
#' the column names to the defaults (`time`, `value`), so that auto-detection
#' can be avoided in future operations.
#'
#' @param x ts-boxable time series, an object of class `ts`, `xts`, `zoo`,
#'   `zooreg`, `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`,
#'   `irts` or `timeSeries`.
#' @return a ts-boxable object of the same class as `x`, i.e., an object of
#'   class `ts`, `xts`, `zoo`, `zooreg`, `data.frame`, `data.table`, `tbl`,
#'   `tbl_ts`, `tbl_time`, `tis`, `irts` or `timeSeries`.
#' @examples
#' \donttest{
#' df <- ts_df(ts_c(mdeaths, fdeaths))
#' # non-default colnames
#' colnames(df) <- c("id", "date", "count")
#' # switch back to default colnames
#' ts_default(df)
#' }
#' @export
#' @srrstats {G2.9} *Software should issue diagnostic messages for type conversion in which information is lost (such as conversion of variables from factor to character; standardisation of variable names; or removal of meta-data such as those associated with [`sf`-format](https://r-spatial.github.io/sf/) data) or added (such as insertion of variable or column names where none were provided).*
#'   Auto detection issues diagnostic messages on [time] and [value] column.
ts_default <- function(x) {
  if (inherits(x, "ts")) {
    return(x)
  }
  z <- ts_dts(x)
  cname <- dts_cname(z)
  if (identical(cname$time, "time") && identical(cname$value, "value")) {
    return(x)
  }
  setnames(z, cname$time, "time")
  setnames(z, cname$value, "value")

  cname$time <- "time"
  cname$value <- "value"

  setcolorder(z, c(setdiff(names(z), c("time", "value")), c("time", "value")))
  setattr(z, "cname", cname)
  copy_class(z, x)
}
