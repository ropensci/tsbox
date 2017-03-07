#' @export
as_ts <- function (x, ...) UseMethod("as_ts")

#' @export
#' @method as_ts xts
as_ts.xts <- function(x) {

  p <- xts::periodicity(x)
  if (p$scale == "monthly"){
    f = 12
  } else if (p$scale == "quarterly"){
    f = 4
  }


  start <- c(as.POSIXlt(index(x[1]))$year + 1900L, as.POSIXlt(index(x[1]))$mon + 1L)

  # TODO extend for bi monthly, semi yearly

  # if (xts::timeBased(x) || !xts::is.xts(x)) 
  #   x <- xts::try.xts(x, error = "'x' needs to be timeBased or xtsible")
  # p <- median(diff(.index(x)))
  # if (is.na(p)) stop("can not calculate periodicity of 1 observation")


  index(x) <- zoo::as.yearmon(index(x))
  as.ts(as.zoo(x))

}
#' @export
#' @method as_ts data.frame
as_ts.data.frame <- function(x, time.name = "time", variable.name = "variable", value.name = "value"){
  as_ts(as_xts(x, time.name = time.name, variable.name = variable.name, value.name = value.name))
}









