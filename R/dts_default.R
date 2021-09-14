#' Default Column Names for 'dts'
#'
#' @param x a 'dts'
#' @noRd
dts_default <- function(x) {
  stopifnot(inherits(x, "dts"))
  cname <- dts_cname(x)
  colorder <- copy(names(x))

  setnames(x, cname$time, "time")
  setnames(x, cname$value, "value")
  setcolorder(x, c(cname$id, "time", "value"))

  cname_default <- cname
  cname_default$time <- "time"
  cname_default$value <- "value"
  setattr(x, "cname", cname_default)

  list(
    x = x,
    cname = cname,
    colorder = colorder
  )
}


#' Default Column Names for 'dts'
#'
#' @param x 'data.table', or 'dts'
#' @param d Attributes to apply on a 'dts'
#'
#' @noRd
dts_restore <- function(x, d) {
  x <- dts_init_minimal(x)
  setnames(x, "time", d$cname$time)
  setnames(x, "value", d$cname$value)
  setcolorder(x, d$colorder)
  setattr(x, "cname", d$cname)
  x
}


#' Minimal 'dts' Initialization
#'
#' Adds 'dts' class attribute to a 'data.table'
#'
#' @param x 'data.table', or 'dts'
#' @noRd
dts_init_minimal <- function(x) {
  stopifnot(inherits(x, "data.table"))
  if (!inherits(x, "dts")) setattr(x, "class", c("dts", attr(x, "class")))
  x
}
