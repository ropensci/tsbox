#' @export
as_ts <- function (x, ...) UseMethod("as_ts")

#' @export
#' @method as_ts xts
as_ts.xts <- function(x) {

  p <- xts::periodicity(x)
  if (p$scale == "monthly"){
    f = 12
    index(x) <- zoo::as.yearmon(index(x))
    z <- as.ts(as.zoo(x))
  } else if (p$scale == "quarterly"){
    f = 4
    index(x) <- zoo::as.yearqtr(index(x))
    z <- as.ts(as.zoo(x))
  } else if (p$scale == "daily"){
    ti <- date_to_time(x)

    f <- length(ti) /  (ti[length(ti)] - ti[1])
    z <- ts(coredata(x), start = ti[1], frequency = f)

  }

  z


  # start <- c(as.POSIXlt(index(x[1]))$year + 1900L, as.POSIXlt(index(x[1]))$mon + 1L)

  # TODO extend for bi monthly, semi yearly


  

}






#' @export
#' @method as_ts data.frame
as_ts.data.frame <- function(x, time.name = "time", variable.name = "variable", value.name = "value"){
  as_ts(as_xts(x, time.name = time.name, variable.name = variable.name, value.name = value.name))
}











