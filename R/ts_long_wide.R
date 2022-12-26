#' Reshaping Multiple Time Series
#'
#' Functions to reshape multiple time series from 'wide' to 'long' and vice
#' versa. Note that long format data frames are ts-boxable objects, where wide
#' format data frames are not. `ts_long` automatically identifies a **time**
#' column, and uses columns on the left as id columns.
#'
#' @param x a ts-boxable time series, or a wide `data.frame`,
#' `data.table`, or `tibble`.
#'
#' @inherit ts_default return
#' @examples
#' x <- ts_df(ts_c(mdeaths, fdeaths))
#' df.wide <- ts_wide(x)
#' df.wide
#' ts_long(df.wide)
#' @export
ts_long <- function(x) {
  rc <- relevant_class(x)
  if (rc %in% c("xts", "ts")) {
    return(x)
  }
  z <- long_core_multi_id(as.data.table(x))
  copy_class(z, x, preserve.names = FALSE)
}


#' Make Wide data.table Long
#'
#' Core function that works on data.table, called by ts_long()
#'
#' @param x data.table
#'
#' @noRd
long_core_multi_id <- function(x) {
  stopifnot(inherits(x, "data.table"))
  time.name <- guess_time(x)
  # guess id: ids on the left of time colum
  all.names <- names(x)
  time.pos <- which(all.names == time.name)
  id.names <- setdiff(all.names[1:time.pos], time.name)
  value.names <- setdiff(all.names[time.pos:length(all.names)], time.name)

  # character cols or factors should be considered ids, with message
  value.classes <- vapply(x[, value.names, with = FALSE], class, "")
  value.names.that.are.ids <- names(value.classes)[value.classes %in% c("character", "factor")]

  if (length(value.names.that.are.ids) > 0) {
    message(
      "found columns right to the [time] column that will be treated as [id] ",
      "columns (character or factor): ",
      paste_quoted(value.names.that.are.ids),
      "."
    )
    value.names <- setdiff(value.names, value.names.that.are.ids)
    id.names <- union(id.names, value.names.that.are.ids)
  }

  if (length(value.names) == 0L) {
    stop0("no [value] column(s) detected. \n[value] column(s) must be right of the [time] column.")
  }
  if (length(id.names) > 0) {
    message(
      "Additional [id] column(s): ",
      paste(paste0("'", id.names, "'"), collapse = ", ")
    )
    id.vars <- c(id.names, time.name)
  } else {
    id.vars <- time.name
  }

  un <- make.unique(c(id.vars, "id"))
  new.id.name <- un[length(un)]

  z <- suppressWarnings(
    melt(x, id.vars = id.vars, variable.name = new.id.name, variable.factor = FALSE)
  )
  setcolorder(z, c(id.names, new.id.name, time.name, "value"))
  ts_dts(z)
}


#' @export
#' @name ts_long
ts_wide <- function(x) {
  check_ts_boxable(x)
  rc <- relevant_class(x)
  if (rc %in% c("ts", "xts", "tbl_time", "tbl_ts", "tis")) {
    return(x)
  }
  x.dts <- combine_id_cols(ts_dts(x))
  z <- wide_core(x.dts)
  # reclass
  rc <- relevant_class(x)
  as_class <- get(paste0("as.", rc))
  as_class(z)
}


#' Make Wide dts a Long data.table
#'
#' Core function that works on dts and data.table, called by ts_wide()
#'
#' @param x dts
#'
#' @noRd
wide_core <- function(x) {
  stopifnot(inherits(x, "dts"))
  if (ncol(x) == 2L) {
    return(x)
  } # nothing to do
  # no multi id
  stopifnot(ncol(x) == 3L)

  cname <- dts_cname(x)

  # tattr <- dts_tattr(x)

  n.non.unique <- nrow(x) - nrow(unique(x, by = c(cname$id, cname$time)))
  if (n.non.unique > 0) {
    stop("contains duplicate entries (this error should not occur.")
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
