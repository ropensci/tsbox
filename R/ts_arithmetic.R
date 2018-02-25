ts_arithmetic <- function(e1, e2, fun = `-`){
  value <- value2 <- NULL
  z1 <- copy(ts_dts(e1))

  # 'recycling', if a scalar is provided
  if (length(e2) == 1 && is.numeric(e2)){
    z2 <- copy(z1)
    z2[[colname_value(z2)]] <- e2
  } else {
    z2 <- copy(ts_dts(e2))
  }
  
  colname.id1 <- colname_id(z1)
  colname.timz1 <- colname_time(z1)
  colname.valuz1 <- colname_value(z1)

  colname.id <- colname_id(z2)
  colname.time <- colname_time(z2)
  colname.value <- colname_value(z2)

  if (!identical(colname.id1, colname.id)){
    stop("id columns are not identical", call. = FALSE)
  }
  if (!identical(colname.timz1, colname.time)){
    stop("time column is not identical", call. = FALSE)
  }
  if (!identical(colname.valuz1, colname.value)){
    stop("value column is not identical", call. = FALSE)
  }

  setnames(z1, colname.value, "value")
  setnames(z2, colname.value, "value2")

  z <- merge(z1, z2, by = c(colname.time, colname.id))
  z[, value := fun(value, value2)]
  z[, value2 := NULL]

  setnames(z, "value", colname.value)
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


