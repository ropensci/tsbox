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
      z1 <- combine_id_cols(z1, sep = sep.str)
      z2 <- combine_id_cols(z2, sep = sep.str)
      cid <- "id"
    }

    ll1 <- split(z1, z1[[cid]])[unique(z1[[cid]])]
    ll2 <- split(z2, z2[[cid]])[unique(z2[[cid]])]

    ll1 <- lapply(ll1, function(e) e[, (cid) := NULL])
    ll2 <- lapply(ll2, function(e) e[, (cid) := NULL])

    z <- rbindlist(Map(merge_time_date, x = ll1, y = ll2), idcol = cid)

    # separate id cols
    if (length(cname$id) > 1){
      sep_id <- function(x){
        spl <- strsplit(x$id, split = sep.str)
        ids <- do.call(data.table, lapply(seq_along(cname$id), function(id) sapply(spl, function(e) e[id])))
        setnames(ids, cname$id)
        x$id <- NULL
        cbind(ids, x)
      }
      z1 <- sep_id(z1)
      z2 <- sep_id(z2)
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


