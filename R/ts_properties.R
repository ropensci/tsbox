#' Extract Time Series Properties
#'
#' These functions do not start with `ts_`, because they don't return an object
#' of the same class as the input.
#' @inherit ts_dts
#'
#' @export
#' @name from
from <- function(x) {
  stopifnot(ts_boxable(x))
  x.dts <- ts_dts(x)
  cname <- dts_cname(x.dts)
  setnames(x.dts, cname$time, "time")
  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
  z <- x.dts[, list(string = time[1]), by = eval(.by)]$string
  id.names <- ids(x.dts)
  if (length(id.names) > 0) names(z) <- ids(x.dts)
  z
}

#' @export
#' @name from
to <- function(x) {
  stopifnot(ts_boxable(x))
  x.dts <- ts_dts(x)
  cname <- dts_cname(x.dts)
  setnames(x.dts, cname$time, "time")
  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
  z <- x.dts[, list(string = time[length(time)]), by = eval(.by)]$string
  id.names <- ids(x.dts)
  if (length(id.names) > 0) names(z) <- ids(x.dts)
  z
}

#' @export
#' @name from
freq <- function(x) {
  stopifnot(ts_boxable(x))
  x.dts <- ts_dts(x)
  cname <- dts_cname(x.dts)
  setnames(x.dts, cname$time, "time")
  .by <- parse(text = paste0("list(", paste(cname$id, collapse = ", "), ")"))
  z <- x.dts[, list(string = frequency_one(time)), by = eval(.by)]$string
  id.names <- ids(x.dts)
  if (length(id.names) > 0) names(z) <- ids(x.dts)
  z
}

#' @export
#' @name from
ids <- function(x, simplify = TRUE) {
  stopifnot(ts_boxable(x))
  x.dts <- ts_dts(x)
  cname <- dts_cname(x.dts)
  x.dts[[cname$value]] <- NULL
  x.dts[[cname$time]] <- NULL
  z <- as.data.frame(unique(x.dts))
  paste_ <- function(...) paste(..., sep = "_")
  do.call(paste_, as.list(z))
}


