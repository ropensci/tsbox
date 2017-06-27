
#' @export
spread_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  z <- dcast(x, time ~ var)
  # keep order as in input
  setcolorder(z, c("time", x[, unique(var)]))
  z
}

#' @export
gather_core <- function(x){
  stopifnot(inherits(x, "data.table"))
  time.name <- guess_time(x)
  z <- melt(x, id.vars = time.name, variable.name = "var", variable.factor = FALSE)
  ts_dts(z)
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