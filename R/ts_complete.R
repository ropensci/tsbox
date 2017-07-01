
# this is a shitty implemenation, should use data.table operations instead


#' Make all time series the same length
#' @param x any time series object
#' @param fill missign value specifier
#' @export
ts_complete <- function(x, fill = NA){
  # DONT go for a "ts" object here!!)

  # TODO, do this propperly

  z <- ts_ts(x)
  if (!is.na(fill)){
    z[is.na(z)] <- fill
  }
  reclass(z, x)
}
