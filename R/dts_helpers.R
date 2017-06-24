
#' @export
spread_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  z <- dcast(x, time ~ var)
  # keep order as in input
  setcolorder(z, c("time", x[, unique(var)]))
  z
}

#' @export
gather_dts <- function(x){
  stopifnot(inherits(x, "data.table"))
  z <- melt(x, id.vars = "time", variable.name = "var", variable.factor = FALSE)
  setcolorder(z, c("time", "value", "var"))
  add_dts_class(z)
}

#' @export
add_dts_class <- function(x){
  class(x) <- c("dts", class(x))
  x
}

#' @export
rm_dts_class <- function(x){
  class(x) <- setdiff(class(x), "dts")
  x
}