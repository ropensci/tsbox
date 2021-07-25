#' Default Column Names
#'
#' In data frame objects (`data.frame`, `tibble`, `data.table`), tsbox
#' automatically detects the time and the value column. This function changes
#' the column names to the defaults (`time`, `value`), so that auto-detection
#' can be avoided in future operations.
#'
#' @inherit ts_dts
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#'
#' df <- ts_df(ts_c(mdeaths, fdeaths))
#' # non-default colnames
#' colnames(df) <- c("id", "date", "count")
#' # switch back to default colnames
#' head(ts_default(df))
#' @export
ts_default <- function(x) {
  if (inherits(x, "ts")) return(x)
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

