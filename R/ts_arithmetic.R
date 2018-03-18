ts_arithmetic <- function(e1, e2, fun = `-`){
  value <- value2 <- NULL
  z1 <- copy(ts_dts(e1))

  if (identical(nrow(z1), 0L)) return(e1)

  # 'recycling', if a scalar is provided
  if (length(e2) == 1 && is.numeric(e2)){
    z2 <- copy(z1)
    z2[[dts_cname(z2)$value]] <- e2
  } else {
    z2 <- copy(ts_dts(e2))
  }

  cname <- dts_cname(z1)
  cname2 <- dts_cname(z2)

  if (!identical(cname$id, cname2$id)){
    stop("id columns are not identical", call. = FALSE)
  }
  if (!identical(cname$time, cname2$time)){
    stop("time column is not identical", call. = FALSE)
  }
  if (!identical(cname$value, cname2$value)){
    stop("value column is not identical", call. = FALSE)
  }

  # the following is quite standard, should be unified and factored out
  setnames(z1, cname$value, "value")
  setnames(z2, cname$value, "value2")
  setnames(z1, cname$time, "time")
  setnames(z2, cname$time, "time")

  is.posixct.z1 <- inherits(z1$time, "POSIXct")
  is.posixct.z2 <- inherits(z1$time, "POSIXct")

  if (is.posixct.z1 || is.posixct.z2){
    tzone <- unique(c(dts_tattr(z1)$tzone), dts_tattr(z2)$tzone)[1]
    z1[, time := as.integer(as.POSIXct(time))]
    z2[, time := as.integer(as.POSIXct(time))]
  }

  z <- merge(z1, z2, by = c(cname$id, "time"))
  z[, value := fun(value, value2)]
  z[, value2 := NULL]

  if (is.posixct.z1 || is.posixct.z2){
    z[, time := as.POSIXct(time, origin = "1970-01-01", tz = tzone)]
  }

  setnames(z, "time", cname$time)
  setnames(z, "value", cname$value)
  setattr(z, "cname", cname)
  copy_class(z, e1)
}


#' Arithmetic Operators for ts-boxable objects
#'
#' @param e1 ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @param e2 ts-boxable time series, an object of class `ts`, `xts`, `data.frame`, `data.table`, or `tibble`.
#' @return a ts-boxable time series, with the same class as the left input.
#' @examples
#' head(fdeaths - mdeaths)
#' head(fdeaths %ts-% mdeaths)
#' head(ts_df(fdeaths) %ts-% mdeaths)
#' @export
#' @name ts_arithmetic
#' @export
`%ts+%` <- function(e1, e2) ts_arithmetic(e1, e2, fun = `+`)

#' @name ts_arithmetic
#' @export
`%ts-%` <- function(e1, e2) ts_arithmetic(e1, e2, fun = `-`)

#' @name ts_arithmetic
#' @export
`%ts*%` <- function(e1, e2) ts_arithmetic(e1, e2, fun = `*`)

#' @name ts_arithmetic
#' @export
`%ts/%` <- function(e1, e2) ts_arithmetic(e1, e2, fun = `/`)


