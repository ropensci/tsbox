
#' @export
spread_dts <- function(x) {
  stopifnot(inherits(x, "dts"))
  time.name <- colnames(x)[1]

  # in a dts, time is always at first position, so no guessing needed

  z <- dcast(x, as.formula(paste(time.name, "~ var")))
  # keep order as in input
  setcolorder(z, c(time.name, x[, unique(var)]))
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