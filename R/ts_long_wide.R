#' Reshaping Multiple Time Series
#'
#' Functions to reshape multiple time series from 'wide' to 'long' and vice versa.
#' Note that long format data frames are ts-boxable objects, where wide format data
#' frames are not. `ts_long` automatically identifies a **time** column, and
#' uses columns on the left as id columns.
#'
#' @param x a ts-boxable time series, or a wide `data.frame`,
#' `data.table`, or `tibble`.
#'
#' @return object with the same class as input
#' @examples
#' df.wide <- ts_wide(ts_df(ts_c(mdeaths, fdeaths)))
#' head(df.wide)
#' head(ts_long(df.wide))
#' @export
ts_long <- function(x) {
  rc <- relevant_class(x)
  if (rc %in% c("xts", "ts")) return(x)
  z <- long_core_multi_id(as.data.table(x))
  copy_class(z, x, preserve.names = FALSE)
}

# a version of long_core that deals with multi id. less robust and not used
# by ts_dts.ts and ts_dts.xts
long_core_multi_id <- function(x) {
  stopifnot(inherits(x, "data.table"))
  time.name <- guess_time(x)
  # guess id: ids on the left of time colum
  all.names <- names(x)
  time.pos <- which(all.names == time.name)
  id.names <- setdiff(all.names[1:time.pos], time.name)
  value.names <- setdiff(all.names[time.pos:length(all.names)], time.name)
  if (length(value.names) == 0) {
    stop("no [value] columns detected (columns right of [time] column)", call. = FALSE)
  }
  if (length(id.names) > 0){
    message("[id] (columns left of [time] column): ",  paste(paste0("'", id.names, "'"), collapse = ", "))
    id.vars <- c(id.names, time.name)
  } else {
    id.vars <- time.name
  }
  z <- suppressWarnings(melt(x, id.vars = id.vars, variable.name = "id", variable.factor = FALSE))
  setcolorder(z, c(id.names, "id", time.name, "value"))
  ts_dts(z)
}

#' @export
#' @name ts_long
ts_wide <- function(x) {
  stopifnot(ts_boxable(x))
  if (relevant_class(x) %in% c("ts", "xts")) return(x)
  x.dts <- combine_id_cols(ts_dts(x))
  z <- wide_core(x.dts)
  # reclass
  rc <- relevant_class(x)
  as_class <- get(paste0("as.", rc))
  as_class(z)
}

wide_core <- function(x) {
  stopifnot(inherits(x, "dts"))
  if (ncol(x) == 2) return(x) # nothing to do
  # no multi id
  stopifnot(ncol(x) == 3)

  cname <- dts_cname(x)

  # tattr <- dts_tattr(x)

  n.non.unique <- nrow(x) - nrow(unique(x, by = c(cname$id, cname$time)))
  if (n.non.unique > 0) {
    stop("contains ", n.non.unique, " duplicate entries", call. = FALSE)
  }

  # dcast is confused by factors
  if (is.factor(x[[cname$id]])) x[[cname$id]] <- as.character(x[[cname$id]])

  setnames(x, cname$time, "time")

  # # dcast is confused by some things
  # cname$id <- gsub("~", "_", cname$id, fixed = TRUE)
  # setnames(x, gsub("~", "_", names(x), fixed = TRUE))
  #
  # Casting works fine for POSIXct as well.
  z <- dcast(
    x,
    as.formula(substitute(time ~ id, list(id = as.name(cname$id)))),
    value.var = cname$value, drop = FALSE
  )
  setnames(z, "time", cname$time)

  # keep order as in input
  setcolorder(z, c(cname$time, unique(as.character(x[[cname$id]]))))
  z
}
