ts_arithmetic <- function(e1, e2, fun = `-`){
  value <- value2 <- .id <- NULL
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
  cid <- cname$id

  if (!identical(cname$id, cname2$id)){
    stop("id columns are not identical", call. = FALSE)
  }

  setnames(z1, cname$value, "value")
  setnames(z2, cname2$value, "value2")
  setnames(z1, cname$time, "time")
  setnames(z2, cname2$time, "time")

  if (length(cname$id) > 0){

    if (length(cname$id) > 1){
      sep.str <- "!%!#"

      cid <- cname$id
      dt.id <- unique(z1[, cid, with = FALSE])

      paste2 <- function(...) paste(..., sep = sep.str)
      dt.id[, .id := do.call(paste2, as.list(dt.id))]

      z1 <- merge(z1, dt.id, by = cid, sort = FALSE)[, (cid) := NULL]
      z2 <- merge(z2, dt.id, by = cid, sort = FALSE)[, (cid) := NULL]
      setnames(z1, ".id", "id")
      setnames(z2, ".id", "id")
      cid <- "id"

    }

    ll1 <- split(z1, z1[[cid]])[unique(z1[[cid]])]
    ll2 <- split(z2, z2[[cid]])[unique(z2[[cid]])]

    ll1 <- lapply(ll1, function(e) e[, (cid) := NULL])
    ll2 <- lapply(ll2, function(e) e[, (cid) := NULL])

    z <- rbindlist(Map(merge_time_date, x = ll1, y = ll2), idcol = cid)

    # separate id cols
    if (length(cname$id) > 1){
      setnames(z, "id", ".id")
      z <- merge(z, dt.id, by = ".id", sort = FALSE)[, .id := NULL]
      setcolorder(z, c(cname$id, "time", "value", "value2"))
    }
  } else {
    z <- merge_time_date(z1, z2)
  }

  z[, value := fun(value, value2)]
  z[, value2 := NULL]
  z <- z[!is.na(value)]

  setnames(z, "time", cname$time)
  setnames(z, "value", cname$value)
  setattr(z, "cname", cname)
  copy_class(z, e1)[]
}


#' Arithmetic Operators for ts-boxable objects
#'
#' @param e1 ts-boxable time series, an object of class `ts`, `xts`, `zoo`,
#'   `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`, `irts` or
#'   `timeSeries`.
#' @param e2 ts-boxable time series, an object of class `ts`, `xts`, `zoo`,
#'   `data.frame`, `data.table`, `tbl`, `tbl_ts`, `tbl_time`, `tis`, `irts` or
#'   `timeSeries`.
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


