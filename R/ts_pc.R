#' First Differences and Percentage Change Rates
#'
#' `ts_pcy` and `ts_diffy` calculate the percentage change rate and the difference
#' compared to the previous period, `ts_pcy` and `ts_diffy` calculate compared to
#' the same period of the previous year.
#'  
#' @inherit ts_dts
#' @return a ts-boxable time series, with the same class as the input.
#' @examples
#' head(ts_diff(ts_c(fdeaths, mdeaths)))
#' head(ts_pc(ts_c(fdeaths, mdeaths)))
#' head(ts_pcy(ts_c(fdeaths, mdeaths)))
#' head(ts_diffy(ts_c(fdeaths, mdeaths)))
#' @export
ts_pc <- function(x) {
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(((z %ts/% ts_lag(z)) %ts-% 1) %ts*% 100, x)
}

#' @name ts_pc
#' @export
ts_diff <- function(x) {
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(z %ts-% ts_lag(z), x)
}

#' @name ts_pc
#' @export
ts_pcy <- function(x) {
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(((z %ts/% ts_lag(z, "1 year")) %ts-% 1) %ts*% 100, x)
}

#' @name ts_pc
#' @export
ts_diffy <- function(x) {
  z <- ts_dts(x)
  z <- ts_regular(z)
  copy_class(z %ts-% ts_lag(z, "1 year"), x)
}

